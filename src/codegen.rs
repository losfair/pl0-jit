use crate::parser::*;
use std::collections::{BTreeSet, BTreeMap};
use dynasmrt::dynasm;
use anyhow::Result;
use thiserror::Error;
use dynasmrt::{DynasmApi, x64::Assembler, AssemblyOffset, DynamicLabel, DynasmLabelApi};
use crate::runtime::Executable;

pub struct TranslationState {
    frames: Vec<Frame>,
    a: Assembler,
    next_proc_id: u32,
    entry: AssemblyOffset,

    read_ext: DynamicLabel,
    write_ext: DynamicLabel,
}

pub struct Frame {
    owner_proc: Identifier,
    owner_proc_id: u32,
    head: DynamicLabel,
    subprocs: Vec<Frame>,
    num_args: usize,
    vars: BTreeSet<Identifier>,
    consts: BTreeMap<Identifier, Integer>,
    raw_locations: BTreeMap<Identifier, i32>,
}

#[derive(Error, Debug)]
pub enum CodegenError {
    #[error("bad integer")]
    BadInteger,

    #[error("not yet implemented")]
    NotImplemented,

    #[error("variable not found")]
    VariableNotFound,

    #[error("value not found")]
    ValueNotFound,

    #[error("procedure not found")]
    ProcNotFound,

    #[error("argument count mismatch")]
    ArgumentCountMismatch,
}

impl TranslationState {
    pub fn new() -> Self {
        let mut a = Assembler::new().unwrap();
        let read_ext = a.new_dynamic_label();
        let write_ext = a.new_dynamic_label();

        dynasm!(a
            ; =>read_ext
            ; mov rax, QWORD (crate::runtime::do_read as i64)
            ; jmp rax // tail call
            ; =>write_ext
            ; mov rax, QWORD (crate::runtime::do_write as i64)
            ; jmp rax // tail call
        );
        TranslationState {
            frames: vec![],
            a,
            next_proc_id: 1,
            entry: AssemblyOffset(0),
            read_ext,
            write_ext,
        }
    }

    pub fn finalize(self) -> Result<Executable> {
        Ok(Executable {
            buffer: self.a.finalize().unwrap(),
            entry: self.entry,
        })
    }

    pub fn generate_program(&mut self, p: &Program) -> Result<()> {
        let root_proc = Proc {
            name: Identifier("<root>".into()),
            args: vec![],
            block: p.block.clone(),
        };
        let root_frame = self.generate_proc(&root_proc)?;
        Ok(())
    }

    pub fn generate_proc(&mut self, p: &Proc) -> Result<Frame> {
        log::debug!("generate_proc: {}", p.name.0);
        let proc_id = self.next_proc_id;
        self.next_proc_id += 1;

        let vars: BTreeSet<_> = p.block.vardecls.iter().cloned().collect();
        let consts: BTreeMap<_, _> = p.block.condecls.iter().cloned().collect();

        // Assign storage
        let raw_locations: BTreeMap<Identifier, i32> = p.args.iter()
            .chain(vars.iter())
            .cloned()
            .zip((16..).step_by(8))
            .collect();

        let head = self.a.new_dynamic_label();

        self.frames.push(Frame {
            owner_proc: p.name.clone(),
            owner_proc_id: proc_id,
            head,
            subprocs: vec![],
            num_args: p.args.len(),
            vars,
            consts,
            raw_locations,
        });

        // Generate subprocs
        for subproc in &p.block.procs {
            let subframe = self.generate_proc(subproc)?;
            self.frames.last_mut().unwrap().subprocs.push(subframe);
        }

        self.entry = self.a.offset();

        // Allocate slots
        dynasm!(self.a
            ; =>head
            ; push rbp
            ; lea rbp, [rsp + 16 + (p.args.len() * 8) as i32]
            ; sub rsp, (p.num_slots() * 8) as i32

            // (function_id, prev_frame)
            ; mov QWORD [rsp + 0], proc_id as i32
            ; mov [rsp + 8], rbp

            // Move rbp to start of arguments
            ; sub rbp, (p.args.len() * 8) as i32
        );

        // Copy arguments
        for i in 0..p.args.len() {
            dynasm!(self.a
                ; mov rax, [rbp + (i as i32) * 8]
                ; mov [rsp + 16 + (i as i32) * 8], rax
            );
        }

        // Reset rbp to start of storage area
        dynasm!(self.a
            ; mov rbp, rsp
        );

        // TODO: Clear vars

        // Generate body
        self.generate_body(&p.block.body)?;

        // Release slots
        dynasm!(self.a
            ; add rsp, (p.num_slots() * 8) as i32
            ; pop rbp
            ; ret
        );

        // Release frame
        Ok(self.frames.pop().unwrap())
    }

    fn last_frame(&self) -> &Frame {
        self.frames.last().unwrap()
    }

    fn generate_body(&mut self, body: &Body) -> Result<()> {
        for stmt in &body.stmts {
            self.generate_stmt(stmt)?;
        }
        Ok(())
    }

    fn generate_stmt(&mut self, stmt: &Stmt) -> Result<()> {
        match stmt.v {
            StmtV::Assign(ref k, ref v) => {
                self.generate_expr(v)?;
                self.write_var(k)?;
            }
            StmtV::If(ref cond, ref t, ref f) => {
                self.generate_l_expr(cond)?;
                
                let else_case = self.a.new_dynamic_label();
                let end = self.a.new_dynamic_label();

                dynasm!(self.a
                    ; cmp rax, 0
                    ; je =>else_case
                );
                self.generate_stmt(t)?;
                dynasm!(self.a
                    ; jmp =>end
                    ; =>else_case
                );
                if let Some(ref f) = f {
                    self.generate_stmt(f)?;
                }
                dynasm!(self.a
                    ; =>end
                );
            }
            StmtV::While(ref cond, ref i) => {
                let head = self.a.new_dynamic_label();
                let end = self.a.new_dynamic_label();

                dynasm!(self.a
                    ; =>head
                );

                self.generate_l_expr(cond)?;
                dynasm!(self.a
                    ; cmp rax, 0
                    ; je =>end
                );

                self.generate_stmt(i)?;

                dynasm!(self.a
                    ; jmp =>head
                );
            }
            StmtV::Call(ref target, ref args) => {
                for arg in args.iter().rev() {
                    self.generate_expr(arg)?;
                    dynasm!(self.a ; push rax);
                }
                let target = self.lookup_call_target(target, args.len())?;
                dynasm!(self.a ; call =>target);
                dynasm!(self.a ; add rsp, (args.len() * 8) as i32);
            }
            StmtV::Body(ref inner) => {
                self.generate_body(inner)?;
            }
            StmtV::Read(ref l) => {
                dynasm!(self.a ; and rsp, -16); // System V stack alignment
                for id in l {
                    dynasm!(self.a ; call =>self.read_ext);
                    self.write_var(id)?;
                }
                dynasm!(self.a ; mov rsp, rbp);
            }
            StmtV::Write(ref l) => {
                dynasm!(self.a ; and rsp, -16); // System V stack alignment
                for e in l {
                    self.generate_expr(e)?;
                    dynasm!(self.a ; mov rdi, rax ; call =>self.write_ext);
                }
                dynasm!(self.a ; mov rsp, rbp);
            }
        }
        Ok(())
    }

    fn generate_l_expr(&mut self, e: &LExpr) -> Result<()> {
        match e.v {
            LExprV::Lop(ref l, ref op, ref r) => {
                self.generate_expr(l)?;
                dynasm!(self.a ; push rax);
                self.generate_expr(r)?;
                dynasm!(self.a ; mov rcx, rax ; pop rdx);
                dynasm!(self.a ; xor eax, eax); // clear upper bits
                dynasm!(self.a ; cmp rdx, rcx);
                match op {
                    Lop::Eq => dynasm!(self.a ; sete al),
                    Lop::Ne => dynasm!(self.a ; setne al),
                    Lop::Lt => dynasm!(self.a ; setl al),
                    Lop::Le => dynasm!(self.a ; setle al),
                    Lop::Gt => dynasm!(self.a ; setg al),
                    Lop::Ge => dynasm!(self.a ; setge al),
                }
            }
            LExprV::Odd(ref e) => {
                self.generate_expr(e)?;
                dynasm!(self.a ; and rax, 1);
            }
        }

        Ok(())
    }

    fn generate_expr(&mut self, e: &Expr) -> Result<()> {
        self.generate_term(&e.left_term)?;

        if let Some((ref aop, ref right)) = e.right_term {
            dynasm!(self.a ; push rax);
            self.generate_term(right)?;
            dynasm!(self.a ; mov rcx, rax ; pop rax);

            match aop {
                Aop::Add => {
                    dynasm!(self.a ; add rax, rcx);
                }
                Aop::Sub => {
                    dynasm!(self.a ; sub rax, rcx);
                }
            }
        }

        if e.neg {
            dynasm!(self.a ; neg rax);
        }

        Ok(())
    }

    fn generate_term(&mut self, t: &Term) -> Result<()> {
        self.generate_factor(&t.left_factor)?;

        if let Some((ref mop, ref right)) = t.right_factor {
            dynasm!(self.a ; push rax);
            self.generate_factor(right)?;
            dynasm!(self.a ; mov rcx, rax ; pop rax);

            match mop {
                Mop::Mul => {
                    dynasm!(self.a ; imul rax, rcx);
                }
                Mop::Div => {
                    dynasm!(self.a ; xor edx, edx ; idiv rcx);
                }
            }
        }

        Ok(())
    }

    fn generate_factor(&mut self, f: &Factor) -> Result<()> {
        match f.v {
            FactorV::Id(ref id) => {
                self.read_value(id)?;
            }
            FactorV::Integer(ref i) => {
                let x: u64 = i.0.parse().map_err(|e| CodegenError::BadInteger)?;
                dynasm!(self.a ; mov rax, QWORD x as i64);
            }
            FactorV::Expr(ref e) => {
                self.generate_expr(&**e)?;
            }
        }
        Ok(())
    }

    fn lookup_call_target(&mut self, t: &Identifier, num_args: usize) -> Result<DynamicLabel> {
        for frame in self.frames.iter().rev() {
            if let Some(target) = frame.subprocs.iter().find(|x| &x.owner_proc == t) {
                if target.num_args != num_args {
                    return Err(CodegenError::ArgumentCountMismatch.into());
                }
                return Ok(target.head.clone());
            }
            if &frame.owner_proc == t {
                if frame.num_args != num_args {
                    return Err(CodegenError::ArgumentCountMismatch.into());
                }
                return Ok(frame.head.clone());
            }
        }

        Err(CodegenError::ProcNotFound.into())
    }

    fn lookup_var_indirect(&mut self, t: &Identifier) -> Result<i32> {
        for frame in self.frames.iter().rev() {
            if let Some(&offset) = frame.raw_locations.get(t) {
                // unwind
                dynasm!(self.a
                    ; mov rcx, rbp
                    ; mov rdx, frame.owner_proc_id as i32
                    ; head:
                    ; mov rcx, [rcx + 8]
                    ; mov r8, [rcx]
                    ; cmp r8, 0
                    ; je >fail
                    ; cmp r8, rdx
                    ; je >ok
                    ; jmp <head
                    ; fail:
                    ; ud2
                    ; ok:
                );
                return Ok(offset);
            }
        }
        return Err(CodegenError::VariableNotFound.into());
    }

    fn write_var(&mut self, t: &Identifier) -> Result<()> {
        match self.last_frame().raw_locations.get(t) {
            Some(&offset) => {
                dynasm!(self.a
                    ; mov [rbp + offset], rax
                );
            }
            None => {
                let offset = self.lookup_var_indirect(t)?;
                dynasm!(
                    self.a
                    ; mov [rcx + offset], rax
                );
            }
        }

        Ok(())
    }

    fn read_value(&mut self, t: &Identifier) -> Result<()> {
        for frame in self.frames.iter().rev() {
            if let Some(&offset) = frame.raw_locations.get(t) {
                return self.read_var(t);
            }
            if let Some(value) = frame.consts.get(t) {
                let x: u64 = value.0.parse().map_err(|e| CodegenError::BadInteger)?;
                dynasm!(self.a ; mov rax, QWORD x as i64);
                return Ok(());
            }
        }
        return Err(CodegenError::ValueNotFound.into());
    }

    fn read_var(&mut self, t: &Identifier) -> Result<()> {
        match self.last_frame().raw_locations.get(t) {
            Some(&offset) => {
                dynasm!(self.a
                    ; mov rax, [rbp + offset]
                );
            }
            None => {
                let offset = self.lookup_var_indirect(t)?;
                dynasm!(
                    self.a
                    ; mov rax, [rcx + offset]
                );
            }
        }

        Ok(())
    }
}

impl Proc {
    pub fn num_slots(&self) -> u32 {
        (
            self.args.len() +
            self.block.vardecls.len() +
            2 // (function_id, prev_frame)
        ) as u32
    }
}