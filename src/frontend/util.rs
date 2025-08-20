use core::panic;
use std::sync::atomic::{AtomicUsize, Ordering};

use koopa::ir::{Type};

use crate::frontend::{ast::*, env::Environment, symbol::Variable};

static BB_COUNTER: AtomicUsize = AtomicUsize::new(0);
static TMP_COUNTER: AtomicUsize = AtomicUsize::new(0);

pub fn fresh_bb_name(prefix: &str) -> String {
  let id = BB_COUNTER.fetch_add(1, Ordering::Relaxed);
  format!("%{}_{}", prefix, id)
}

pub fn fresh_tmp_name() -> String {
  let id = TMP_COUNTER.fetch_add(1, Ordering::Relaxed);
  format!("%__sc_tmp{}", id)
}

pub fn get_var_type(size: &Vec<ConstExp>, env: &mut Environment) -> Type {
  if size.is_empty() {
    Type::get_i32()
  } else {
    let mut ty = Type::get_i32();
    for exp in size.iter().rev() {
      let len = exp.eval(env);
      ty = Type::get_array(ty, len as usize);
    }
    ty
  }
}

pub(super) trait Eval {
  fn eval(&self, env: &mut Environment) -> i32;
}

/// Evaluate a constant expression
impl Eval for ConstExp {
  fn eval(&self, env: &mut Environment) -> i32 {
    let folded = self.fold(env);
    match folded {
      ConstExp::Exp(Exp::Number(n)) => n,
      _ => panic!("Cannot evaluate expression: {:?}", folded),
    }
  }
}

impl Eval for Exp {
  fn eval(&self, env: &mut Environment) -> i32 {
    let folded = self.fold(env);
    match folded {
      Exp::Number(n) => n,
      _ => panic!("Cannot evaluate expression: {:?}", folded),
    }
  }
}

pub(super) trait Fold {
  fn fold(&self, env: &mut Environment) -> Self;
}

impl Fold for ConstExp {
  fn fold(&self, env: &mut Environment) -> ConstExp {
    match self {
      ConstExp::Exp(exp) => ConstExp::Exp(exp.fold(env)),
    }
  }
}

impl Fold for Exp {
  fn fold(&self, env: &mut Environment) -> Exp {
    match self {
      Exp::Number(n) => Exp::Number(*n),

      Exp::LValExp(lval) => match env.table.get_var(&lval.ident) {
        Some(Variable::Const(value)) => Exp::Number(*value),
        Some(Variable::Var(_var)) => Exp::LValExp(LValExp { ident: lval.ident.to_string(), index: vec![] }),
        Some(Variable::ConstArray(_) | Variable::Array(_)) => self.clone(),
        None => panic!("Variable {} not found in symbol table", lval.ident),
      },

      Exp::Unary { op, exp } => {
        let e = exp.fold(env);
        if let Exp::Number(n) = e {
          Exp::Number(match op {
            UnaryOp::Pos => n,
            UnaryOp::Neg => -n,
            UnaryOp::Not => (n == 0) as i32,
          })
        } else {
          Exp::Unary { op: op.clone(), exp: Box::new(e) }
        }
      },

      Exp::Binary { op, lhs, rhs } => {
        let l = lhs.fold(env);
        let r = rhs.fold(env);
        if let (Exp::Number(a), Exp::Number(b)) = (&l, &r) {
          Exp::Number(match op {
            BinaryOp::Add => a + b,
            BinaryOp::Sub => a - b,
            BinaryOp::Mul => a * b,
            BinaryOp::Div => a / b,
            BinaryOp::Mod => a % b,
            BinaryOp::Lt => (a < b) as i32,
            BinaryOp::Gt => (a > b) as i32,
            BinaryOp::Le => (a <= b) as i32,
            BinaryOp::Ge => (a >= b) as i32,
            BinaryOp::Eq => (a == b) as i32,
            BinaryOp::Ne => (a != b) as i32,
          })
        } else {
          Exp::Binary { op: op.clone(), lhs: Box::new(l), rhs: Box::new(r) }
        }
      }

      Exp::ShortCircuit { op, lhs, rhs } => {
        let l = lhs.fold(env);
        let r = rhs.fold(env);
        if let (Exp::Number(a), Exp::Number(b)) = (&l, &r) {
          Exp::Number(match op {
            ShortCircuitOp::And => ((*a != 0) && (*b != 0)) as i32,
            ShortCircuitOp::Or => ((*a != 0) || (*b != 0)) as i32,
          })
        } else if let Exp::Number(a) = &l {
          match op {
            ShortCircuitOp::And => {
              if *a != 0 {
                r
              } else {
                Exp::Number(0)
              }
            }
            ShortCircuitOp::Or => {
              if *a != 0 {
                Exp::Number(1)
              } else {
                r
              }
            }
          }
        } else {
          Exp::ShortCircuit { op: op.clone(), lhs: Box::new(l), rhs: Box::new(r) }
        }
      }
      _ => self.clone(),
    }
  }
}