use crate::frontend::symbol::SymbolTable;
use koopa::ir::{builder::{BasicBlockBuilder, LocalBuilder}, BasicBlock, Function, FunctionData, Program, Value};

#[derive(Default)]
pub struct Environment {
  pub table: SymbolTable,
  pub ctx: Context,
}

#[derive(Default)]
pub struct Context {
  pub program: Program,
  pub func: Option<Function>,
  pub block: Option<BasicBlock>,
}

impl Context {
  pub fn set_func(&mut self, func: Function) {
    self.func = Some(func);
  }

  pub fn set_block(&mut self, bb: BasicBlock) {
    self.block = Some(bb);
  }

  pub fn func_data(&mut self) -> &mut FunctionData {
    self.program.func_mut(self.func.expect("No function set"))
  }

  pub fn local_builder(&mut self) -> LocalBuilder {
    self.func_data().dfg_mut().new_value()
  }

  pub fn create_block(&mut self, name: Option<String>) {
    let block = self.func_data().dfg_mut().new_bb().basic_block(name);
    self.func_data().layout_mut().bbs_mut().push_key_back(block).unwrap();
    self.set_block(block);
  }

  pub fn add_inst(&mut self, inst: Value) {
    let block = self.block.expect("No block set");
    self.func_data().layout_mut().bb_mut(block).insts_mut().push_key_back(inst).unwrap();
  }
}