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
  pub items: Vec<BlockItem>,
}

#[derive(Debug)]
pub enum BlockItem {
  Decl(Decl),
  Stmt(Stmt),
}

#[derive(Debug)]
pub enum Decl {
  Const(ConstDecl),
  Var(VarDecl),
}

#[derive(Debug)]
pub struct ConstDecl {
  pub btype: BType,
  pub defs: Vec<ConstDef>,
}

#[derive(Debug)]
pub struct VarDecl {
  pub btype: BType,
  pub defs: Vec<VarDef>,
}

#[derive(Debug)]
pub enum BType {
  Int,
}

#[derive(Debug)]
pub struct ConstDef {
    pub ident: String,
    pub init: ConstInitVal,
}

#[derive(Debug)]
pub struct VarDef {
    pub ident: String,
    pub init: Option<InitVal>,
}

#[derive(Debug)]
pub enum ConstInitVal {
  Exp(Exp),
}

#[derive(Debug)]
pub enum InitVal {
  Exp(Exp),
}

#[derive(Debug)]
pub enum Stmt {
  Return(Exp),
  Assign { lval: LVal, exp: Exp },
}

#[derive(Debug)]
pub enum LVal {
  Var(String),
}

#[derive(Debug)]
pub enum Exp {
  Number(i32),
  Unary {
    op: UnaryOp,
    exp: Box<Exp>
  },
  Binary {
    op: BinaryOp,
    lhs: Box<Exp>,
    rhs: Box<Exp>
  },
  LVal(LVal)
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
  Pos,
  Neg,
  Not
}

#[derive(Debug, Clone)]
pub enum BinaryOp {
  Add, Sub, Mul, Div, Mod,
  Lt, Gt, Le, Ge,
  Eq, Ne,
  And, Or,
}