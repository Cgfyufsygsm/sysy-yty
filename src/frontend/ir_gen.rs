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
    if env.ctx.current_open_block().is_none() {
      // If no block is open, we cannot generate any instructions.
      return;
    }
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
    if env.ctx.current_open_block().is_none() {
      // If no block is open, we cannot generate any instructions.
      return;
    }
    match self {
      Stmt::Return(exp) => {
        let exp = exp.fold(env);
        let ret_val = exp.generate_on(env);
        let ret_inst = env.ctx.local_builder().ret(Some(ret_val));
        env.ctx.add_inst(ret_inst);
        env.ctx.mark_block_terminated(env.ctx.block.expect("No block set"));
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
      Stmt::While(while_stmt) => {
        while_stmt.generate_on(env);
      }
      Stmt::Break(break_stmt) => {
        break_stmt.generate_on(env);
      }
      Stmt::Continue(continue_stmt) => {
        continue_stmt.generate_on(env);
      }
      _ => {}
    }
  }
}

impl GenerateIR for If {
  type Output = ();
  
  fn generate_on(&self, env: &mut Environment) {
    let cond = self.cond.fold(env).generate_on(env);
    let orig_bb = env.ctx.block.expect("No current basic block when generating if");


    match &self.else_block {
      None => {
        env.ctx.create_block(Some(fresh_bb_name("then")));
        let then_bb = env.ctx.block.expect("Failed to create 'then' block");

        env.ctx.create_block(Some(fresh_bb_name("end")));
        let end_bb = env.ctx.block.expect("Failed to create 'end' block");

        env.ctx.set_block(orig_bb);
        let branch_inst = env.ctx.local_builder().branch(cond, then_bb, end_bb);
        env.ctx.add_inst(branch_inst);
        env.ctx.mark_block_terminated(orig_bb);

        // then
        env.ctx.set_block(then_bb);
        self.then_block.generate_on(env);
        let then_open = env.ctx.current_open_block();

        if let Some(bb) = then_open {
          env.ctx.set_block(bb);
          let jump_inst = env.ctx.local_builder().jump(end_bb);
          env.ctx.add_inst(jump_inst);
          env.ctx.mark_block_terminated(bb);
        }

        env.ctx.set_block(end_bb);
      }

      Some(else_stmt) => {
        env.ctx.create_block(Some(fresh_bb_name("then")));
        let then_bb = env.ctx.block.expect("then not set");

        env.ctx.create_block(Some(fresh_bb_name("else")));
        let else_bb = env.ctx.block.expect("else not set");

        env.ctx.set_block(orig_bb);
        let branch_inst = env.ctx.local_builder().branch(cond, then_bb, else_bb);
        env.ctx.add_inst(branch_inst);
        env.ctx.mark_block_terminated(orig_bb);

        // then
        env.ctx.set_block(then_bb);
        self.then_block.generate_on(env);
        let then_open = env.ctx.current_open_block(); // 保存 then 的开放尾块

        env.ctx.set_block(else_bb);
        else_stmt.generate_on(env);
        let else_open = env.ctx.current_open_block(); // 保存 else 的开放尾块

        if then_open.is_none() && else_open.is_none() {
          // 如果两个分支都没有开放尾块，则不需要创建结束块
          env.ctx.clear_block();
          return;
        }

        env.ctx.create_block(Some(fresh_bb_name("end")));
        let merge_bb = env.ctx.block.expect("end not set");

        if let Some(then_bb) = then_open {
          env.ctx.set_block(then_bb);
          let jump_inst = env.ctx.local_builder().jump(merge_bb);
          env.ctx.add_inst(jump_inst);
          env.ctx.mark_block_terminated(then_bb);
        }

        if let Some(else_bb) = else_open {
          env.ctx.set_block(else_bb);
          let jump_inst = env.ctx.local_builder().jump(merge_bb);
          env.ctx.add_inst(jump_inst);
          env.ctx.mark_block_terminated(else_bb);
        }

        env.ctx.set_block(merge_bb);
      }
    }
  }
}

impl GenerateIR for While {
  type Output = ();

  fn generate_on(&self, env: &mut Environment) {
    let orig_bb = env.ctx.block.expect("No current basic block when generating while");

    env.ctx.create_block(Some(fresh_bb_name("while_entry")));
    let while_entry_bb = env.ctx.block.expect("Failed to create 'while_entry' block");
    env.ctx.create_block(Some(fresh_bb_name("while_body")));
    let while_body_bb = env.ctx.block.expect("Failed to create 'while_body' block");
    env.ctx.create_block(Some(fresh_bb_name("while_end")));
    let while_end_bb = env.ctx.block.expect("Failed to create 'while_end' block");

    env.ctx.set_block(orig_bb);
    let jump_entry_inst = env.ctx.local_builder().jump(while_entry_bb);
    env.ctx.add_inst(jump_entry_inst);
    env.ctx.mark_block_terminated(orig_bb);

    env.ctx.set_block(while_entry_bb);
    let cond = self.cond.fold(env).generate_on(env);
    let cur_bb = env.ctx.block.expect("No current basic block after condition evaluation");
    env.ctx.set_block(cur_bb);
    let branch_inst = env.ctx.local_builder().branch(cond, while_body_bb, while_end_bb);
    env.ctx.add_inst(branch_inst);
    env.ctx.mark_block_terminated(cur_bb);

    // push loop into stack
    env.ctx.push_loop(while_entry_bb, while_end_bb);

    env.ctx.set_block(while_body_bb);
    self.body.generate_on(env);
    let body_open = env.ctx.current_open_block();

    env.ctx.pop_loop();

    if body_open.is_some() {
      let body_jmp_inst = env.ctx.local_builder().jump(while_entry_bb);
      env.ctx.add_inst(body_jmp_inst);
      env.ctx.mark_block_terminated(while_body_bb);
    }
    // Set the end block as the current block
    env.ctx.set_block(while_end_bb);

  }
}

impl GenerateIR for Break {
  type Output = ();

  fn generate_on(&self, env: &mut Environment) {
    if let Some(loop_info) = env.ctx.current_loop() {
      let break_bb = loop_info.break_bb;
      let break_inst = env.ctx.local_builder().jump(break_bb);
      env.ctx.add_inst(break_inst);
      env.ctx.mark_block_terminated(env.ctx.block.expect("No block set"));
      env.ctx.clear_block();
    } else {
      panic!("Break statement outside of loop");
    }
  }
}

impl GenerateIR for Continue {
  type Output = ();

  fn generate_on(&self, env: &mut Environment) {
    if let Some(loop_info) = env.ctx.current_loop() {
      let continue_bb = loop_info.continue_bb;
      let continue_inst = env.ctx.local_builder().jump(continue_bb);
      env.ctx.add_inst(continue_inst);
      env.ctx.mark_block_terminated(env.ctx.block.expect("No block set"));
      env.ctx.clear_block();
    } else {
      panic!("Continue statement outside of loop");
    }
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
      // 要求当前已经有一个 open block
        let orig_bb = env.ctx.block.expect("No current basic block when generating short-circuit");

        // eval lhs
        let lhs_val = lhs.generate_on(env);
        let zero = env.ctx.local_builder().integer(0);
        let lhs_bool = env.ctx.local_builder().binary(NotEq, lhs_val, zero);
        env.ctx.add_inst(lhs_bool);

        // allocate a temp to hold the final boolean result (i32)
        let res_ptr = env.ctx.local_builder().alloc(Type::get_i32());
        env.ctx.add_inst(res_ptr);

        match op {
          ShortCircuitOp::And => {
            // blocks: rhs (when lhs true), false (when lhs false), merge
            env.ctx.create_block(Some(fresh_bb_name("and_rhs")));
            let rhs_bb = env.ctx.block.expect("and_rhs not set");
            env.ctx.create_block(Some(fresh_bb_name("and_false")));
            let false_bb = env.ctx.block.expect("and_false not set");
            env.ctx.create_block(Some(fresh_bb_name("and_end")));
            let merge_bb = env.ctx.block.expect("and_end not set");

            // branch on lhs_bool: true -> rhs_bb, false -> false_bb
            env.ctx.set_block(orig_bb);
            let branch_inst = env.ctx.local_builder().branch(lhs_bool, rhs_bb, false_bb);
            env.ctx.add_inst(branch_inst);
            env.ctx.mark_block_terminated(orig_bb);

            // rhs_bb: evaluate rhs, compute rhs_bool, store into res_ptr, jump merge
            env.ctx.set_block(rhs_bb);
            let rhs_val = rhs.generate_on(env);
            let rhs_bool = env.ctx.local_builder().binary(NotEq, rhs_val, zero);
            env.ctx.add_inst(rhs_bool);
            let store_rhs = env.ctx.local_builder().store(rhs_bool, res_ptr);
            env.ctx.add_inst(store_rhs);
            let jump_to_merge = env.ctx.local_builder().jump(merge_bb);
            env.ctx.add_inst(jump_to_merge);
            env.ctx.mark_block_terminated(rhs_bb);

            // false_bb: store 0, jump merge
            env.ctx.set_block(false_bb);
            let store_false = env.ctx.local_builder().store(zero, res_ptr);
            env.ctx.add_inst(store_false);
            let jump_false = env.ctx.local_builder().jump(merge_bb);
            env.ctx.add_inst(jump_false);
            env.ctx.mark_block_terminated(false_bb);

            // merge: load and return
            env.ctx.set_block(merge_bb);
            let load_inst = env.ctx.local_builder().load(res_ptr);
            env.ctx.add_inst(load_inst);
            load_inst
          }
          ShortCircuitOp::Or => {
            // blocks: true (when lhs true), rhs (when lhs false), merge
            env.ctx.create_block(Some(fresh_bb_name("or_true")));
            let true_bb = env.ctx.block.expect("or_true not set");
            env.ctx.create_block(Some(fresh_bb_name("or_rhs")));
            let rhs_bb = env.ctx.block.expect("or_rhs not set");
            env.ctx.create_block(Some(fresh_bb_name("or_end")));
            let merge_bb = env.ctx.block.expect("or_end not set");

            // branch on lhs_bool: true -> true_bb, false -> rhs_bb
            env.ctx.set_block(orig_bb);
            let branch_inst = env.ctx.local_builder().branch(lhs_bool, true_bb, rhs_bb);
            env.ctx.add_inst(branch_inst);
            env.ctx.mark_block_terminated(orig_bb);

            // true_bb: store 1, jump merge
            env.ctx.set_block(true_bb);
            let one = env.ctx.local_builder().integer(1);
            let store_true = env.ctx.local_builder().store(one, res_ptr);
            env.ctx.add_inst(store_true);
            let jump_true = env.ctx.local_builder().jump(merge_bb);
            env.ctx.add_inst(jump_true);
            env.ctx.mark_block_terminated(true_bb);

            // rhs_bb: evaluate rhs, compute rhs_bool, store into res_ptr, jump merge
            env.ctx.set_block(rhs_bb);
            let rhs_val = rhs.generate_on(env);
            let rhs_bool = env.ctx.local_builder().binary(NotEq, rhs_val, zero);
            env.ctx.add_inst(rhs_bool);
            let store_rhs = env.ctx.local_builder().store(rhs_bool, res_ptr);
            env.ctx.add_inst(store_rhs);
            let jump_to_merge = env.ctx.local_builder().jump(merge_bb);
            env.ctx.add_inst(jump_to_merge);
            env.ctx.mark_block_terminated(rhs_bb);

            // merge: load and return
            env.ctx.set_block(merge_bb);
            let load_inst = env.ctx.local_builder().load(res_ptr);
            env.ctx.add_inst(load_inst);
            load_inst
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