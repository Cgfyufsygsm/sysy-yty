use super::ast::*;
use koopa::ir::{builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder}, BasicBlock, FunctionData, Program, Type, Value, Function};

impl CompUnit {
  pub fn generate_on(&self, program: &mut Program) {
    self.func_def.generate_on(program);
  }
}

impl FuncDef {
  pub fn generate_on(&self, program: &mut Program) {
      // Create a new function.

      let func_data = FunctionData::new(
        format!("@{}", self.ident),
        vec![],
        match self.func_type {
          FuncType::Int => Type::get_i32(),
        },
      );
      
      let func = program.new_func(func_data);

        // Create an entry basic block.
      let entry_bb = program.func_mut(func).dfg_mut().new_bb().basic_block(Some("%entry".into()));
      program.func_mut(func).layout_mut().bbs_mut().push_key_back(entry_bb).unwrap();

      // Generate IR for the block.
      self.block.generate_on(program, func, entry_bb);

      // Add the function to the program.
    }
}

impl Block {
  pub fn generate_on(&self, program: &mut Program, func: Function, entry_bb: BasicBlock) {
    self.stmt.generate_on(program, func, entry_bb);
  }
}


impl Stmt {
  pub fn generate_on(&self, program: &mut Program, func: Function, entry_bb: BasicBlock) {
    let ret_val = program.func_mut(func).dfg_mut().new_value().integer(self.num);
    let ret_inst = program.func_mut(func).dfg_mut().new_value().ret(Some(ret_val));
    program.func_mut(func).layout_mut().bb_mut(entry_bb).insts_mut().push_key_back(ret_inst).unwrap();
  }
}