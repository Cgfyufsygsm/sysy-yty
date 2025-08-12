#[derive(Debug)]
pub struct CompUnit {
  pub func_def: FuncDef,
}

#[derive(Debug)]
pub struct FuncDef {
  pub func_type: FuncType,
  pub ident: String,
  // pub params: Vec<Param>,
  pub block: Block,
}

#[derive(Debug)]
pub enum FuncType {
    Int,
}

#[derive(Debug)]
pub struct Block {
    pub stmt: Stmt,
}

#[derive(Debug)]
pub enum Stmt {
    Return(Exp),
}

#[derive(Debug)]
pub enum Exp {
  Unary {
    op: UnaryOp,
    exp: Box<Exp>
  },
  Number(i32),
}

#[derive(Debug)]
pub enum UnaryOp {
  Pos,
  Neg,
  Not
}