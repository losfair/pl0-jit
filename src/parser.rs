

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Program {
    pub name: Identifier,
    pub block: Block,
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Block {
    pub condecls: Vec<(Identifier, Integer)>,
    pub vardecls: Vec<Identifier>,
    pub procs: Vec<Proc>,
    pub body: Body,
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Proc {
    pub name: Identifier,
    pub args: Vec<Identifier>,
    pub block: Block,
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Body {
    pub stmts: Vec<Stmt>,
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Stmt {
    pub v: StmtV,
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum StmtV {
    Assign(Identifier, Expr),
    If(LExpr, Box<Stmt>, Box<Stmt>),
    While(LExpr, Box<Stmt>),
    Call(Identifier, Vec<Expr>),
    Body(Body),
    Read(Vec<Identifier>),
    Write(Vec<Expr>),
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct LExpr {
    pub v: LExprV,
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum LExprV {
    Lop(Expr, Lop, Expr),
    Odd(Expr),
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Expr {
    pub neg: bool,
    pub left_term: Term,
    pub right_term: Option<(Aop, Term)>,
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Term {
    pub left_factor: Factor,
    pub right_factor: Option<(Mop, Factor)>,
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Factor {
    pub v: FactorV,
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum FactorV {
    Id(Identifier),
    Integer(Integer),
    Expr(Box<Expr>),
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum Lop {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum Aop {
    Add,
    Sub,
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum Mop {
    Mul,
    Div,
}

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Identifier(pub String);

#[derive(Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub struct Integer(pub String);

peg::parser! {
    pub grammar parser() for str {
        rule integer() -> Integer
            = n:$(['0'..='9']+) { Integer(n.into()) }

        rule id() -> Identifier
            = x:$((['a'..='z'] / ['A'..='Z']) (['a'..='z'] / ['A'..='Z'] / ['0'..='9'])*) { Identifier(x.into()) }

        rule mop() -> Mop
            =
                "*" { Mop::Mul } /
                "/" { Mop::Div }

        rule aop() -> Aop
            =
                "+" { Aop::Add } /
                "-" { Aop::Sub }

        rule lop() -> Lop
            =
                "=" { Lop::Eq } /
                "<>" { Lop::Ne } /
                "<" { Lop::Lt } /
                "<=" { Lop::Le } /
                ">" { Lop::Gt } /
                ">=" { Lop::Ge }

        rule factor_v() -> FactorV
            =
                x:id() { FactorV::Id(x) } /
                x:integer() { FactorV::Integer(x) } /
                "(" _ e:expr() _ ")" { FactorV::Expr(Box::new(e)) }

        rule factor() -> Factor
            =
                v:factor_v() { Factor { v } }

        rule expr() -> Expr
            =
                "+" _ i:expr_inner() { Expr { neg: false, left_term: i.0, right_term: i.1 } } /
                "-" _ i:expr_inner() { Expr { neg: true, left_term: i.0, right_term: i.1 } } /
                i:expr_inner() { Expr { neg: false, left_term: i.0, right_term: i.1 } }

        rule expr_inner() -> (Term, Option<(Aop, Term)>)
            =
                l_term:term() _ op:aop() _ r_term:term() { (l_term, Some((op, r_term))) } /
                l_term:term() { (l_term, None) }

        rule term() -> Term
            =
                left_factor:factor() _ op:mop() _ right_factor:factor() { Term { left_factor, right_factor: Some((op, right_factor)) } } /
                left_factor:factor() { Term { left_factor, right_factor: None } }

        rule l_expr() -> LExpr
            =
                "odd" _ e:expr() { LExpr { v: LExprV::Odd(e) } } /
                l:expr() _ op:lop() _ r:expr() { LExpr { v: LExprV::Lop(l, op, r) } }

        rule stmt_v() -> StmtV
            =
                l:id() _ ":=" _ r:expr() { StmtV::Assign(l, r) } /
                "if" _ cond:l_expr() _ "then" _ l:stmt() _ "else" _ r:stmt() { StmtV::If(cond, Box::new(l), Box::new(r)) } /
                "while" _ cond:l_expr() _ "do" _ l:stmt() { StmtV::While(cond, Box::new(l)) } /
                "call" _ t:id() _ "(" _ e:(_ x:expr() _ {x}) ** "," _ ")" { StmtV::Call(t, e) } /
                "read" _ "(" _ i:(_ x:id() _ {x}) ** "," _ ")" { StmtV::Read(i) } /
                "write" _ "(" _ e:(_ x:expr() _ {x}) ** "," _ ")" { StmtV::Write(e) } /
                b:body() { StmtV::Body(b) }

        rule stmt() -> Stmt
            = v:stmt_v() { Stmt { v } }

        rule body() -> Body
            = "begin" _ stmts:(_ x:stmt() _ {x}) ** ";" _ "end" { Body { stmts } }

        rule procedure() -> Proc
            = "procedure" _ name:id() _ "(" _ args:(_ x:id() _ {x}) ** "," _ ")" _ ";" _ b:block() { Proc { name, args, block: b} }

        rule block() -> Block
            = c:condecls()? _ v:vardecls()? _ p:((_ p:procedure() _ {p}) ** ";")? _ b:body() {
                Block {
                    condecls: c.unwrap_or(vec![]),
                    vardecls: v.unwrap_or(vec![]),
                    procs: p.unwrap_or(vec![]),
                    body: b,
                }
            }

        rule condecls() -> Vec<(Identifier, Integer)>
            = "const" _ e:(_ i:id() _ ":=" _ v:integer() _ { (i, v) }) ** "," _ ";" { e }

        rule vardecls() -> Vec<Identifier>
            = "var" _ i:(_ i:id() _ {i}) ** "," _ ";" { i }

        pub rule program() -> Program
            = "program" _ name:id() _ ";" _ block:block() { Program { name, block } }

        rule _() =  quiet!{[' ' | '\t' | '\n' | '\r']*}
    }
}