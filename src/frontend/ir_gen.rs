use crate::frontend::{env::Environment, symbol::Variable};

use super::ast::*;
use koopa::ir::{builder::{LocalInstBuilder, ValueBuilder}, FunctionData, Type, Value, BinaryOp::*};

pub trait GenerateIR {
  type Output;
  fn generate_on(&self, env: &mut Environment) -> Self::Output;
}

impl GenerateIR for CompUnit {
  type Output = ();

  fn generate_on(&self, env: &mut Environment) {
    self.func_def.generate_on(env);
  }
}

impl GenerateIR for FuncDef {
  type Output = ();

  fn generate_on(&self, env: &mut Environment) {
    // Create a new function.
    env.table.enter_scope();

    let func_data = FunctionData::new(
      format!("@{}", self.ident),
      vec![],
      match self.func_type {
        FuncType::Int => Type::get_i32(),
      },
    );
    
    let func = env.ctx.program.new_func(func_data);
    env.ctx.set_func(func);
    env.ctx.create_block(Some("@entry".to_string()));

    // Generate IR for the block.
    self.block.generate_on(env);
    
    env.table.leave_scope();
  }
}

impl GenerateIR for Block {
  type Output = ();
  fn generate_on(&self, env: &mut Environment) {
    for item in &self.items {
      item.generate_on(env);
    }
  }
}

impl GenerateIR for BlockItem {
  type Output = ();
  fn generate_on(&self, env: &mut Environment) {
    match self {
      BlockItem::Decl(decl) => decl.generate_on(env),
      BlockItem::Stmt(stmt) => stmt.generate_on(env),
    }
  }
}

impl GenerateIR for Decl {
  type Output = ();

  fn generate_on(&self, env: &mut Environment) {
    match self {
      Decl::Const(const_decl) => const_decl.generate_on(env),
    }
  }
}

impl GenerateIR for ConstDecl {
  type Output = ();

  fn generate_on(&self, env: &mut Environment) {
    for def in &self.defs {
      def.generate_on(env);
    }
  }
}

impl GenerateIR for ConstDef {
  type Output = ();

  fn generate_on(&self, env: &mut Environment) {
    let value = match &self.init {
      ConstInitVal::Exp(exp) => exp.fold(env),
    };
    env.table.insert_const(&self.ident, match value {
      Exp::Number(n) => n,
      _ => panic!("Expected constant expression, found {:?}", value),
    });
  }
}

impl GenerateIR for Stmt {
  type Output = ();

  fn generate_on(&self, env: &mut Environment) {
    match self {
      Stmt::Return(exp) => {
        let exp = exp.fold(env);
        let ret_val = exp.generate_on(env);
        let ret_inst = env.ctx.local_builder().ret(Some(ret_val));
        env.ctx.add_inst(ret_inst);
      }
    }
  }
}

trait Fold {
  fn fold(&self, env: &mut Environment) -> Self;
}

impl Fold for Exp {
  fn fold(&self, env: &mut Environment) -> Exp {
    match self {
      Exp::Number(n) => Exp::Number(*n),

      Exp::Var(ident) => match env.table.get(&ident) {
        Some(Variable::Const(value)) => Exp::Number(*value),
        Some(Variable::Var) => Exp::Var(ident.to_string()),
        None => panic!("Variable {} not found in symbol table", ident),
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
            BinaryOp::And => ((*a != 0) && (*b != 0)) as i32,
            BinaryOp::Or => ((*a != 0) || (*b != 0)) as i32,
          })
        } else {
          Exp::Binary { op: op.clone(), lhs: Box::new(l), rhs: Box::new(r) }
        }
      }
    }
  }
}

impl GenerateIR for Exp {
  type Output = Value;

  fn generate_on(&self, env: &mut Environment) -> Value {

    match self {
      Exp::Number(n) => {
          env.ctx.local_builder().integer(*n)
        }
      Exp::Unary { op, exp} => {
        let val = exp.generate_on(env);

        let inst = match op {
          UnaryOp::Pos => return val,
          UnaryOp::Neg => {
            let zero = env.ctx.local_builder().integer(0);
            let sub_inst = env.ctx.local_builder().binary(Sub, zero, val);
            sub_inst
          }
          UnaryOp::Not => {
            let zero = env.ctx.local_builder().integer(0);
            let cmp_inst = env.ctx.local_builder().binary(Eq, val, zero);
            cmp_inst
          }
        };
        env.ctx.add_inst(inst);
        inst
      }
      Exp::Binary { op, lhs, rhs} => {
        let lhs_val = lhs.generate_on(env);
        let rhs_val = rhs.generate_on(env);

        if let BinaryOp::And = op {
          let zero = env.ctx.local_builder().integer(0);
          let lhs_bool = env.ctx.local_builder().binary(NotEq, lhs_val, zero);
          env.ctx.add_inst(lhs_bool);
          let rhs_bool = env.ctx.local_builder().binary(NotEq, rhs_val, zero);
          env.ctx.add_inst(rhs_bool);
          let inst = env.ctx.local_builder().binary(And, lhs_bool, rhs_bool);
          env.ctx.add_inst(inst);
          return inst;
        }

        if let BinaryOp::Or = op {
          let zero = env.ctx.local_builder().integer(0);
          let lhs_bool = env.ctx.local_builder().binary(NotEq, lhs_val, zero);
          env.ctx.add_inst(lhs_bool);
          let rhs_bool = env.ctx.local_builder().binary(NotEq, rhs_val, zero);
          env.ctx.add_inst(rhs_bool);
          let inst = env.ctx.local_builder().binary(Or, lhs_bool, rhs_bool);
          env.ctx.add_inst(inst);
          return inst;
        }

        let kind = match op {
          BinaryOp::Add => Add,
          BinaryOp::Sub => Sub,
          BinaryOp::Mul => Mul,
          BinaryOp::Div => Div,
          BinaryOp::Mod => Mod,
          BinaryOp::Lt => Lt,
          BinaryOp::Gt => Gt,
          BinaryOp::Le => Le,
          BinaryOp::Ge => Ge,
          BinaryOp::Eq => Eq,
          BinaryOp::Ne => NotEq,
          _ => {
            panic!("Unsupported binary operation: {:?}", op);
          }
        };
        let inst = env.ctx.local_builder().binary(kind, lhs_val, rhs_val);
        env.ctx.add_inst(inst);
        inst
      }

      Exp::Var(ident) => {
        match env.table.get(ident) {
          Some(Variable::Const(value)) => {
            env.ctx.local_builder().integer(*value)
          }
          // Some(Variable::Var) => {
          //   let local = env.ctx.local_builder().local(ident.clone());
          //   env.ctx.add_inst(local);
          //   local
          // }
          // None => panic!("Variable {} not found in symbol table", ident),
          _ => panic!("unimplemented error")
        }
      }
    }
  }
}