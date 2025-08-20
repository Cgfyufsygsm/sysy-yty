#[derive(Debug, Clone)]
pub struct CompUnit {
  pub comp_items: Vec<CompItem>,
}

#[derive(Debug, Clone)]
pub enum CompItem {
  FuncDef(FuncDef),
  Decl(Decl),
}

#[derive(Debug, Clone)]
pub struct FuncDef {
  pub btype: BType,
  pub ident: String,
  pub params: Vec<FuncParam>,
  pub block: Block,
}

#[derive(Debug, Clone)]
pub enum BType {
  Int,
  Void,
}

#[derive(Debug, Clone)]
pub struct FuncParam {
  pub btype: BType,
  pub ident: String,
}

#[derive(Debug, Clone)]
pub struct Block {
  pub items: Vec<BlockItem>,
}

#[derive(Debug, Clone)]
pub enum BlockItem {
  Decl(Decl),
  Stmt(Stmt),
}

#[derive(Debug, Clone)]
pub enum Decl {
  Const(ConstDecl),
  Var(VarDecl),
}

#[derive(Debug, Clone)]
pub struct ConstDecl {
  pub btype: BType,
  pub defs: Vec<ConstDef>,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
  pub btype: BType,
  pub defs: Vec<VarDef>,
}

#[derive(Debug, Clone)]
pub struct ConstDef {
  pub ident: String,
  pub size: Vec<ConstExp>,
  pub init: ConstInitVal,
}

#[derive(Debug, Clone)]
pub struct VarDef {
  pub ident: String,
  pub size: Vec<ConstExp>,
  pub init: Option<InitVal>,
}

#[derive(Debug, Clone)]
pub enum ConstInitVal {
  ConstExp(ConstExp),
  ConstInitVals(Box<Vec<ConstInitVal>>),
}

#[derive(Debug, Clone)]
pub enum InitVal {
  Exp(Exp),
  InitVals(Box<Vec<InitVal>>),
}

#[derive(Debug, Clone)]
pub enum Stmt {
  Return(Option<Exp>),
  Assign(Assign),
  Exp(Option<Exp>),
  Block(Block),
  If(If),
  While(While),
  Break(Break),
  Continue(Continue),
}

#[derive(Debug, Clone)]
pub struct Assign {
  pub lval: LValAssign,
  pub exp: Exp,
}

#[derive(Debug, Clone)]
pub struct LValAssign {
  pub ident: String,
  pub index: Vec<Exp>,
}

#[derive(Debug, Clone)]
pub struct Break;

#[derive(Debug, Clone)]
pub struct Continue;

#[derive(Debug, Clone)]
pub struct If {
  pub cond: Exp,
  pub then_block: Box<Stmt>,
  pub else_block: Option<Box<Stmt>>,
}

#[derive(Debug, Clone)]
pub struct While {
  pub cond: Exp,
  pub body: Box<Stmt>,
}

#[derive(Debug, Clone)]
pub struct LValExp {
  pub ident: String,
  pub index: Vec<Exp>,
}

#[derive(Debug, Clone)]
pub enum ConstExp {
  Exp(Exp),
}

#[derive(Debug, Clone)]
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
  ShortCircuit {
    op: ShortCircuitOp,
    lhs: Box<Exp>,
    rhs: Box<Exp>
  },
  LValExp(LValExp),
  Call(Call)
}

#[derive(Debug, Clone)]
pub struct Call {
  pub func: String,
  pub args: Vec<Exp>,
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
}

#[derive(Debug, Clone)]
pub enum ShortCircuitOp {
  And,
  Or,
}