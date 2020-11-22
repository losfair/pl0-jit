use crate::parser::*;
use std::collections::{BTreeSet, BTreeMap};
use dynasmrt::dynasm;
use anyhow::Result;
use thiserror::Error;
use dynasmrt::{DynasmApi, x64::Assembler, DynasmLabelApi};
use crate::runtime::Executable;

pub struct TranslationState {
    frames: Vec<Frame>,
    a: Assembler,
    next_proc_id: u32,
}

pub struct Frame {
    owner_proc: Identifier,
    owner_proc_id: u32,
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
}

impl TranslationState {
    pub fn new() -> Self {
        TranslationState {
            frames: vec![],
            a: Assembler::new().unwrap(),
            next_proc_id: 1,
        }
    }

    pub fn finalize(self) -> Result<Executable> {
        Ok(Executable {
            buffer: self.a.finalize().unwrap(),
        })
    }

    pub fn generate_program(&mut self, p: &Program) -> Result<()> {
        let root_proc = Proc {
            name: Identifier("<root>".into()),
            args: vec![],
            block: p.block.clone(),
        };
        self.generate_proc(&root_proc)
    }

    pub fn generate_proc(&mut self, p: &Proc) -> Result<()> {
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

        self.frames.push(Frame {
            owner_proc: p.name.clone(),
            owner_proc_id: proc_id,
            vars,
            consts,
            raw_locations,
        });

        // Allocate slots
        dynasm!(self.a
            ; push rbp
            ; lea rbp, [rsp + 16]
            ; sub rsp, (p.num_slots() * 8) as i32

            // (function_id, prev_frame)
            ; mov QWORD [rsp + 0], proc_id as i32
            ; mov [rsp + 8], rbp
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

        // Generate subprocs
        for subproc in &p.block.procs {
            self.generate_proc(subproc)?;
        }

        // Release frame
        self.frames.pop().unwrap();

        Ok(())
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
            _ => return Err(CodegenError::NotImplemented.into()),
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