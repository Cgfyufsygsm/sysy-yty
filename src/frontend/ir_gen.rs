use crate::frontend::{env::Environment, symbol::Variable, util::{Fold, fresh_bb_name, fresh_tmp_name}};

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
  }
}

impl GenerateIR for Block {
  type Output = ();
  fn generate_on(&self, env: &mut Environment) {
    env.table.enter_scope();
    for item in &self.items {
      item.generate_on(env);
    }
    env.table.leave_scope();
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
      Decl::Var(var_decl) => var_decl.generate_on(env),
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

impl GenerateIR for VarDecl {
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

impl GenerateIR for VarDef {
  type Output = ();

  fn generate_on(&self, env: &mut Environment) {
    let ptr = env.ctx.local_builder().alloc(Type::get_i32());
    env.ctx.add_inst(ptr);

    env.table.insert_var(&self.ident, ptr);

    if let Some(init) = &self.init {
      let val = match init {
        InitVal::Exp(exp) => exp.fold(env).generate_on(env)
      };
      let store_inst = env.ctx.local_builder().store(val, ptr);
      env.ctx.add_inst(store_inst);
    }
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
      Stmt::Assign { lval, exp } => {
        let ptr = lval.generate_on(env);
        let val = exp.fold(env).generate_on(env);
        let store_inst = env.ctx.local_builder().store(val, ptr);
        env.ctx.add_inst(store_inst);
      }
      Stmt::Exp(opt_exp) => {
        if let Some(e) = opt_exp {
          e.fold(env).generate_on(env);
        }
      }
      Stmt::Block(block) => {
        block.generate_on(env);
      }
      Stmt::If(if_stmt) => {
        if_stmt.generate_on(env);
      }
    }
  }
}

impl GenerateIR for If {
  type Output = ();
  
  fn generate_on(&self, env: &mut Environment) {
    let orig_bb = env.ctx.block.expect("No current basic block when generating if");
    let cond = self.cond.fold(env).generate_on(env);
    
    let then_name = fresh_bb_name("then");
    env.ctx.create_block(Some(then_name));
    let then_bb = env.ctx.block.expect("Failed to create 'then' block");

    let else_name = fresh_bb_name("else");
    env.ctx.create_block(Some(else_name));
    let else_bb = env.ctx.block.expect("Failed to create 'else' block");

    let end_name = fresh_bb_name("end");
    env.ctx.create_block(Some(end_name));
    let end_bb = env.ctx.block.expect("Failed to create 'end' block");

    // set back to original block and generate branch instruction
    env.ctx.set_block(orig_bb);
    let branch_inst = env.ctx.local_builder().branch(cond, then_bb, else_bb);
    env.ctx.add_inst(branch_inst);
    env.ctx.mark_block_terminated(orig_bb);

    // Generate the 'then' block
    env.ctx.set_block(then_bb);
    self.then_block.generate_on(env);
    if !env.ctx.is_block_terminated(then_bb) {
      let jump_inst = env.ctx.local_builder().jump(end_bb);
      env.ctx.add_inst(jump_inst);
      env.ctx.mark_block_terminated(then_bb);
    }

    // Generate the 'else' block if it exists
    env.ctx.set_block(else_bb);
    if let Some(else_stmt) = &self.else_block {
      else_stmt.generate_on(env);

      if !env.ctx.is_block_terminated(else_bb) {
        let jump_inst = env.ctx.local_builder().jump(end_bb);
        env.ctx.add_inst(jump_inst);
        env.ctx.mark_block_terminated(else_bb);
      }
    } else {
      // If there's no else block, we still need to jump to the end
      let jump_inst = env.ctx.local_builder().jump(end_bb);
      env.ctx.add_inst(jump_inst);
      env.ctx.mark_block_terminated(else_bb);
    }

    env.ctx.set_block(end_bb);
  }
}

impl GenerateIR for LVal {
  type Output = Value;

  fn generate_on(&self, env: &mut Environment) -> Value {
    match self {
      LVal::Var(ident) => {
        match env.table.get(ident) {
          Some(Variable::Var(ptr)) => *ptr,
          Some(Variable::Const(value)) => env.ctx.local_builder().integer(*value),
          None => panic!("Variable {} not found in symbol table", ident),
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
        };
        let inst = env.ctx.local_builder().binary(kind, lhs_val, rhs_val);
        env.ctx.add_inst(inst);
        inst
      }

      Exp::ShortCircuit { op, lhs, rhs } => {
        let tmp_name = fresh_tmp_name();
        let result_ptr = env.ctx.local_builder().alloc(Type::get_i32());
        env.ctx.add_inst(result_ptr);
        // alloc tmp res ptr
        env.table.insert_var(&tmp_name, result_ptr);
        match op {
          ShortCircuitOp::And => {
            // if lhs != 0, then tmp <- rhs != 0
            let then_assign = Stmt::Assign {
              lval: LVal::Var(tmp_name.clone()),
              exp: Exp::Binary {
                op: BinaryOp::Ne,
                lhs: rhs.clone(),
                rhs: Box::new(Exp::Number(0)),
              },
            };
            // else tmp <- 0
            let else_assign = Stmt::Assign {
              lval: LVal::Var(tmp_name.clone()),
              exp: Exp::Number(0),
            };

            let cond = Exp::Binary { op: BinaryOp::Ne, lhs: lhs.clone(), rhs: Box::new(Exp::Number(0)) };

            If {
              cond,
              then_block: Box::new(then_assign),
              else_block: Some(Box::new(else_assign)),
            }.generate_on(env);
            env.ctx.local_builder().load(result_ptr)
          }
          ShortCircuitOp::Or => {
            // if lhs != 0, then tmp <- 1
            let then_assign = Stmt::Assign {
              lval: LVal::Var(tmp_name.clone()),
              exp: Exp::Number(1),
            };
            // else tmp <- rhs != 0
            let else_assign = Stmt::Assign {
              lval: LVal::Var(tmp_name.clone()),
              exp: Exp::Binary {
                op: BinaryOp::Ne,
                lhs: rhs.clone(),
                rhs: Box::new(Exp::Number(0)),
              },
            };

            let cond = Exp::Binary { op: BinaryOp::Ne, lhs: lhs.clone(), rhs: Box::new(Exp::Number(0)) };

            If {
              cond,
              then_block: Box::new(then_assign),
              else_block: Some(Box::new(else_assign)),
            }.generate_on(env);
            env.ctx.local_builder().load(result_ptr)
          }
        }
      }

      Exp::LVal(lval) => {
        match lval {
          LVal::Var(ident) => {
            match env.table.get(ident) {
              Some(Variable::Const(value)) => {
                env.ctx.local_builder().integer(*value)
              }
              Some(Variable::Var(ptr)) => {
                let load_inst = env.ctx.local_builder().load(*ptr);
                env.ctx.add_inst(load_inst);
                load_inst
              }
              None => panic!("Variable {} not found in symbol table", ident),
            }
          }
        }
      }
    }
  }
}