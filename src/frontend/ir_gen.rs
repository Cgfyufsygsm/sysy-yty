use core::panic;

use crate::frontend::{env::Environment, symbol::Variable, util::{fresh_bb_name, get_var_type, Eval, Fold}, array_init::*};

use super::ast::*;
use koopa::ir::{BinaryOp::*, FunctionData, Type, TypeKind, Value};

/// 计算给定符号变量和 indices 后的“元素地址（element pointer）”
/// - `var` 是从 symbol table 得到的 Variable
/// - `indices` 是 AST 的索引表达式列表（可以为空）
///
/// 返回：最终的 element pointer（Value），供 store 或读取地址使用
fn compute_elem_ptr_from_var(env: &mut Environment, var: &Variable, indices: &[Exp]) -> Value {
  match var {
    Variable::Var(ptr) => {
      // 非数组普通变量，只能在没有 index 时直接返回其地址
      if !indices.is_empty() {
        panic!("variable is not an array but indices provided");
      }
      *ptr
    }
    Variable::Array(ptr) | Variable::ConstArray(ptr) => {
      let mut elem_ptr = *ptr;
      for index in indices {
        let idx_val = index.fold(env).generate_on(env);
        elem_ptr = env.ctx.get_elem_ptr(elem_ptr, idx_val);
        env.ctx.add_inst(elem_ptr);
      }
      elem_ptr
    }
    Variable::Ptr(ptr) => {
      // pointer variable stores an address; first load the pointer value
      let load_inst = env.ctx.local_load(*ptr);
      env.ctx.add_inst(load_inst);

      if indices.is_empty() {
        // 如果没有 indices，直接返回 load 的结果
        return load_inst;
      }

      let mut elem_ptr = load_inst;
      for (i, index) in indices.iter().enumerate() {
        let idx_val = index.fold(env).generate_on(env);
        if i == 0 {
          elem_ptr = env.ctx.get_ptr(elem_ptr, idx_val);
        } else {
          elem_ptr = env.ctx.get_elem_ptr(elem_ptr, idx_val);
        }
        env.ctx.add_inst(elem_ptr);
      }
      elem_ptr
    }
    Variable::Const(_) => {
      panic!("Const used where address expected");
    }
  }
}

/// 给定一个 address（可能是 pointer-to-array 或 pointer-to-int），根据目标类型决定：
/// - 如果是 pointer -> array : decay（get_elem_ptr(addr, 0)），返回 pointer-to-first-element
/// - 如果是 pointer -> int   : load(addr) 返回值
fn load_or_decay_if_needed(env: &mut Environment, ptr: Value) -> Value {
  let cur_data_ty = env.ctx.get_value_ty(ptr);
  if let TypeKind::Pointer(ty) = cur_data_ty.kind() {
    match ty.kind() {
      TypeKind::Array(_, _) => {
        let zero = env.ctx.local_integer(0);
        let elem_ptr = env.ctx.get_elem_ptr(ptr, zero);
        env.ctx.add_inst(elem_ptr);
        elem_ptr
      }
      TypeKind::Int32 => {
        let load_inst = env.ctx.local_load(ptr);
        env.ctx.add_inst(load_inst);
        load_inst
      }
      _ => unreachable!("unexpected element type: {:?}", ty),
    }
  } else {
    unreachable!("Expected pointer type for address, found: {:?}", cur_data_ty);
  }
}

pub trait GenerateIR {
  type Output;
  fn generate_on(&self, env: &mut Environment) -> Self::Output;
}

impl GenerateIR for CompUnit {
  type Output = ();

  fn generate_on(&self, env: &mut Environment) {
    // setup builtin
    super::sysy_lib::setup_sysy_lib(env);

    for item in &self.comp_items {
      item.generate_on(env);
    }
  }
}

impl GenerateIR for CompItem {
  type Output = ();

  fn generate_on(&self, env: &mut Environment) {
    match self {
      CompItem::FuncDef(func_def) => func_def.generate_on(env),
      CompItem::Decl(decl) => decl.generate_on(env),
    }
  }
}

impl GenerateIR for FuncDef {
  type Output = ();

  fn generate_on(&self, env: &mut Environment) {
    env.table.enter_scope();

    let mut params = vec![];
    for param in &self.params {
      let ident = format!("@{}", param.ident);
      params.push((Some(ident), param.get_type(env)));
    }
    let func_data = FunctionData::with_param_names(
      format!("@{}", self.ident),
      params.clone(), 
      self.btype.clone().into()
    );

    
    let func = env.ctx.program.new_func(func_data);
    env.table.insert_func(&self.ident, func);
    env.ctx.set_func(func);
    env.ctx.create_block(Some(format!("@{}_entry", self.ident)));

    for (i, param) in self.params.iter().enumerate() {
      let value = env.ctx.func_data().params()[i];
      env.ctx.set_value_name(value, param.ident.clone());

      let param_ty = param.get_type(env);
      let alloc = env.ctx.alloc_and_store(value, param_ty.clone());
      env.table.insert_local(&param.ident, alloc, param_ty, false);
    }
    

    // Generate IR for the block.
    self.block.generate_on(env);

    env.table.leave_scope();

    if env.ctx.current_open_block().is_some() {
      // if no explicit return in the function, return a default value
      let default_ret = match self.btype {
        BType::Void => None,
        BType::Int => Some(env.ctx.local_integer(0)),
      };
      let ret = env.ctx.ret(default_ret);
      env.ctx.add_inst(ret);
      env.ctx.mark_block_terminated(env.ctx.block.expect("No block set"));
    }

    env.ctx.clear_func();
    env.ctx.clear_block();
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
    if env.ctx.current_open_block().is_none() && !env.ctx.is_global() {
      // If no block is open, we should not generate any instructions.
      // But we still need to generate the declaration of global variables.
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
    let ty = get_var_type(&self.size, env);

    if env.ctx.is_global() {
      // 处理全局常量
      match &self.init {
        ConstInitVal::ConstExp(exp) => {
          // 单个变量的情况
          let value = exp.eval(env);
          env.table.insert_global_const(&self.ident, value);
        }
        ConstInitVal::ConstInitVals(_) => {
          let init = generate_global_init_vals(env, ty.clone(), &self.init);
          let alloc = env.ctx.global_alloc(init);
          env.ctx.set_global_name(alloc, self.ident.clone());
          env.table.insert_global(&self.ident, alloc, ty.clone(), true);
        }
      }
    } else {
      // 处理局部常量
      match &self.init {
        ConstInitVal::ConstExp(exp) => {
          // 单个变量的情况
          let value = exp.eval(env);
          env.table.insert_const(&self.ident, value);
        }
        ConstInitVal::ConstInitVals(_) => {
          let ptr = env.ctx.local_alloc(ty.clone());
          env.ctx.add_inst(ptr);
          env.table.insert_local(&self.ident, ptr, ty.clone(), true);
          env.ctx.set_value_name(ptr, self.ident.clone());
          generate_local_init_vals(env, ty, ptr, &self.init);
        }
      }
    }
  }
}

impl GenerateIR for VarDef {
  type Output = ();

  fn generate_on(&self, env: &mut Environment) {
    let ty = get_var_type(&self.size, env);

    if env.ctx.is_global() {
      let init = if let Some(init) = &self.init {
        match init {
          InitVal::Exp(exp) => exp.fold(env).generate_on(env),
          InitVal::InitVals(_) => generate_global_init_vals(env, ty.clone(), init),
        }
      } else {
        env.ctx.global_zero_init(ty.clone())
      };
      let alloc = env.ctx.global_alloc(init);
      env.ctx.set_global_name(alloc, self.ident.clone());
      env.table.insert_global(&self.ident, alloc, ty.clone(), false);
    } else {
      // 处理局部变量
      let ptr = env.ctx.local_alloc(ty.clone());
      env.ctx.add_inst(ptr);

      env.table.insert_local(&self.ident, ptr, ty.clone(), false);
      env.ctx.set_value_name(ptr, self.ident.clone());

      if let Some(init) = &self.init {
        match init {
          InitVal::Exp(exp) => {
            let val = exp.fold(env).generate_on(env);
            let store_inst = env.ctx.local_store(val, ptr);
            env.ctx.add_inst(store_inst);
          }
          InitVal::InitVals(_) => {
            generate_local_init_vals(env, ty.clone(), ptr, init);
          }
        };
      }
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
        let ret_inst = if let Some(exp) = exp {
          let ret_val = exp.fold(env).generate_on(env);
          env.ctx.ret(Some(ret_val))
        } else {
          // If no expression, return void.
          env.ctx.ret(None)
        };
        env.ctx.add_inst(ret_inst);
        env.ctx.mark_block_terminated(env.ctx.block.expect("No block set"));
      }
      Stmt::Assign(assign) => assign.generate_on(env),
      Stmt::Exp(opt_exp) => {
        opt_exp.as_ref().map(|e| e.fold(env).generate_on(env));
      }
      Stmt::Block(block) => block.generate_on(env),
      Stmt::If(if_stmt) => if_stmt.generate_on(env),
      Stmt::While(while_stmt) => while_stmt.generate_on(env),
      Stmt::Break(break_stmt) => break_stmt.generate_on(env),
      Stmt::Continue(continue_stmt) => continue_stmt.generate_on(env),
    }
  }
}

impl GenerateIR for Assign {
  type Output = ();

  fn generate_on(&self, env: &mut Environment) -> Self::Output {
    let ptr = self.lval.generate_on(env);
    let val = self.exp.fold(env).generate_on(env);
    let store_inst = env.ctx.local_store(val, ptr);
    env.ctx.add_inst(store_inst);
  }
}

impl GenerateIR for LValAssign {
  type Output = Value;

  fn generate_on(&self, env: &mut Environment) -> Value {
    let var = env.table.get_var(&self.ident).cloned();
    match var {
      Some(Variable::Const(_)) => panic!("Cannot assign to constant variable: {}", self.ident),
      Some(Variable::ConstArray(_)) => panic!("Cannot assign to constant array: {}", self.ident),
      Some(var) => compute_elem_ptr_from_var(env, &var, &self.index),
      None => panic!("Variable {} not found in symbol table", self.ident),
    }
  }
}

impl GenerateIR for If {
  type Output = ();
  
  fn generate_on(&self, env: &mut Environment) {
    let cond_folded = self.cond.fold(env);
    if let Exp::Number(value) = cond_folded {
      if value != 0 {
        self.then_block.generate_on(env);
      } else if let Some(else_stmt) = &self.else_block {
        else_stmt.generate_on(env);
      }
      return;
    }
    let cond = cond_folded.generate_on(env);
    let orig_bb = env.ctx.block.expect("No current basic block when generating if");


    match &self.else_block {
      None => {
        env.ctx.create_block(Some(fresh_bb_name("then")));
        let then_bb = env.ctx.block.expect("Failed to create 'then' block");

        env.ctx.create_block(Some(fresh_bb_name("end")));
        let end_bb = env.ctx.block.expect("Failed to create 'end' block");

        env.ctx.set_block(orig_bb);
        let branch_inst = env.ctx.branch(cond, then_bb, end_bb);
        env.ctx.add_inst(branch_inst);
        env.ctx.mark_block_terminated(orig_bb);

        // then
        env.ctx.set_block(then_bb);
        self.then_block.generate_on(env);
        let then_open = env.ctx.current_open_block();

        if let Some(bb) = then_open {
          env.ctx.set_block(bb);
          let jump_inst = env.ctx.jump(end_bb);
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
        let branch_inst = env.ctx.branch(cond, then_bb, else_bb);
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
          let jump_inst = env.ctx.jump(merge_bb);
          env.ctx.add_inst(jump_inst);
          env.ctx.mark_block_terminated(then_bb);
        }

        if let Some(else_bb) = else_open {
          env.ctx.set_block(else_bb);
          let jump_inst = env.ctx.jump(merge_bb);
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

    let cond_folded = self.cond.fold(env);
    if let Exp::Number(value) = cond_folded {
      if value == 0 {
        return;
      }

      env.ctx.create_block(Some(fresh_bb_name("while_body")));
      let while_body_bb = env.ctx.block.expect("Failed to create 'while_body' block");
      env.ctx.create_block(Some(fresh_bb_name("while_end")));
      let while_end_bb = env.ctx.block.expect("Failed to create 'while_end' block");

      env.ctx.set_block(orig_bb);
      let jump_body_inst = env.ctx.jump(while_body_bb);
      env.ctx.add_inst(jump_body_inst);
      env.ctx.mark_block_terminated(orig_bb);

      // push loop into stack
      env.ctx.push_loop(while_body_bb, while_end_bb);

      env.ctx.set_block(while_body_bb);
      self.body.generate_on(env);
      let body_open = env.ctx.current_open_block();

      env.ctx.pop_loop();

      if let Some(bb) = body_open {
        env.ctx.set_block(bb);
        let body_jmp_inst = env.ctx.jump(while_body_bb);
        env.ctx.add_inst(body_jmp_inst);
        env.ctx.mark_block_terminated(bb);
      }

      env.ctx.set_block(while_end_bb);
      return;
    }

    env.ctx.create_block(Some(fresh_bb_name("while_entry")));
    let while_entry_bb = env.ctx.block.expect("Failed to create 'while_entry' block");
    env.ctx.create_block(Some(fresh_bb_name("while_body")));
    let while_body_bb = env.ctx.block.expect("Failed to create 'while_body' block");
    env.ctx.create_block(Some(fresh_bb_name("while_end")));
    let while_end_bb = env.ctx.block.expect("Failed to create 'while_end' block");

    env.ctx.set_block(orig_bb);
    let jump_entry_inst = env.ctx.jump(while_entry_bb);
    env.ctx.add_inst(jump_entry_inst);
    env.ctx.mark_block_terminated(orig_bb);

    env.ctx.set_block(while_entry_bb);
    let cond = cond_folded.generate_on(env);
    let cur_bb = env.ctx.block.expect("No current basic block after condition evaluation");
    env.ctx.set_block(cur_bb);
    let branch_inst = env.ctx.branch(cond, while_body_bb, while_end_bb);
    env.ctx.add_inst(branch_inst);
    env.ctx.mark_block_terminated(cur_bb);

    // push loop into stack
    env.ctx.push_loop(while_entry_bb, while_end_bb);

    env.ctx.set_block(while_body_bb);
    self.body.generate_on(env);
    let body_open = env.ctx.current_open_block();

    env.ctx.pop_loop();

    if body_open.is_some() {
      let body_jmp_inst = env.ctx.jump(while_entry_bb);
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
      let break_inst = env.ctx.jump(break_bb);
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
      let continue_inst = env.ctx.jump(continue_bb);
      env.ctx.add_inst(continue_inst);
      env.ctx.mark_block_terminated(env.ctx.block.expect("No block set"));
      env.ctx.clear_block();
    } else {
      panic!("Continue statement outside of loop");
    }
  }
}

impl GenerateIR for Exp {
  type Output = Value;

  fn generate_on(&self, env: &mut Environment) -> Value {

    match self {
      Exp::Number(n) => {
        if env.ctx.is_global() {
          return env.ctx.global_integer(*n);
        }
        env.ctx.local_integer(*n)
      }
      Exp::Unary { op, exp} => {
        let val = exp.generate_on(env);

        let inst = match op {
          UnaryOp::Pos => return val,
          UnaryOp::Neg => {
            let zero = env.ctx.local_integer(0);
            let sub_inst = env.ctx.binary(Sub, zero, val);
            sub_inst
          }
          UnaryOp::Not => {
            let zero = env.ctx.local_integer(0);
            let cmp_inst = env.ctx.binary(Eq, val, zero);
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
        let inst = env.ctx.binary(kind, lhs_val, rhs_val);
        env.ctx.add_inst(inst);
        inst
      }

      Exp::ShortCircuit { op, lhs, rhs } => {
        // eval lhs
        let lhs_val = lhs.generate_on(env);
        let zero = env.ctx.local_integer(0);
        let lhs_bool = env.ctx.binary(NotEq, lhs_val, zero);
        env.ctx.add_inst(lhs_bool);

        // allocate a temp to hold the final boolean result (i32)
        let res_ptr = env.ctx.local_alloc(Type::get_i32());
        env.ctx.add_inst(res_ptr);

        let orig_bb = env.ctx.block.expect("No current basic block when generating short-circuit");

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
            let branch_inst = env.ctx.branch(lhs_bool, rhs_bb, false_bb);
            env.ctx.add_inst(branch_inst);
            env.ctx.mark_block_terminated(orig_bb);

            // rhs_bb: evaluate rhs, compute rhs_bool, store into res_ptr, jump merge
            env.ctx.set_block(rhs_bb);
            let rhs_val = rhs.generate_on(env);
            let rhs_bool = env.ctx.binary(NotEq, rhs_val, zero);
            env.ctx.add_inst(rhs_bool);
            let store_rhs = env.ctx.local_store(rhs_bool, res_ptr);
            env.ctx.add_inst(store_rhs);
            let jump_to_merge = env.ctx.jump(merge_bb);
            env.ctx.add_inst(jump_to_merge);
            env.ctx.mark_block_terminated(rhs_bb);

            // false_bb: store 0, jump merge
            env.ctx.set_block(false_bb);
            let store_false = env.ctx.local_store(zero, res_ptr);
            env.ctx.add_inst(store_false);
            let jump_false = env.ctx.jump(merge_bb);
            env.ctx.add_inst(jump_false);
            env.ctx.mark_block_terminated(false_bb);

            // merge: load and return
            env.ctx.set_block(merge_bb);
            let load_inst = env.ctx.local_load(res_ptr);
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
            let branch_inst = env.ctx.branch(lhs_bool, true_bb, rhs_bb);
            env.ctx.add_inst(branch_inst);
            env.ctx.mark_block_terminated(orig_bb);

            // true_bb: store 1, jump merge
            env.ctx.set_block(true_bb);
            let one = env.ctx.local_integer(1);
            let store_true = env.ctx.local_store(one, res_ptr);
            env.ctx.add_inst(store_true);
            let jump_true = env.ctx.jump(merge_bb);
            env.ctx.add_inst(jump_true);
            env.ctx.mark_block_terminated(true_bb);

            // rhs_bb: evaluate rhs, compute rhs_bool, store into res_ptr, jump merge
            env.ctx.set_block(rhs_bb);
            let rhs_val = rhs.generate_on(env);
            let rhs_bool = env.ctx.binary(NotEq, rhs_val, zero);
            env.ctx.add_inst(rhs_bool);
            let store_rhs = env.ctx.local_store(rhs_bool, res_ptr);
            env.ctx.add_inst(store_rhs);
            let jump_to_merge = env.ctx.jump(merge_bb);
            env.ctx.add_inst(jump_to_merge);
            env.ctx.mark_block_terminated(rhs_bb);

            // merge: load and return
            env.ctx.set_block(merge_bb);
            let load_inst = env.ctx.local_load(res_ptr);
            env.ctx.add_inst(load_inst);
            load_inst
          }
        }
      }

      Exp::LValExp(lval) => lval.generate_on(env),
      Exp::Call(call) => call.generate_on(env),
    }
  }
}

impl GenerateIR for LValExp {
  type Output = Value;
  
  fn generate_on(&self, env: &mut Environment) -> Value {
    let var = env.table.get_var(&self.ident).cloned();
    match var {
      Some(Variable::Const(value)) => {
        env.ctx.local_integer(value)
      }
      Some(Variable::Ptr(ptr)) => {
        if self.index.is_empty() {
          // 对于 int f(int a[]) {return g(a);} 类型的特判
          let load_inst = env.ctx.local_load(ptr);
          env.ctx.add_inst(load_inst);
          load_inst
        } else {
          let ptr = compute_elem_ptr_from_var(env, &Variable::Ptr(ptr), &self.index);
          load_or_decay_if_needed(env, ptr)
        }
      }
      Some(var) => {
        let ptr = compute_elem_ptr_from_var(env, &var, &self.index);
        load_or_decay_if_needed(env, ptr)
      }
      None => panic!("Variable {} not found in symbol table", self.ident),
    }
  }
}

impl GenerateIR for Call {
  type Output = Value;

  fn generate_on(&self, env: &mut Environment) -> Value {
    let mut args = vec![];

    for arg in &self.args {
      let arg_val = arg.fold(env).generate_on(env);
      args.push(arg_val);
    }
    let func = env.table.get_func(&self.func).expect("Function not found in symbol table");

    let call_inst = env.ctx.call(*func, args);
    env.ctx.add_inst(call_inst);

    call_inst
  }
}
