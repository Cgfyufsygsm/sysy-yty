use std::collections::HashSet;

use crate::frontend::symbol::SymbolTable;
use koopa::ir::{builder::{BasicBlockBuilder, LocalBuilder}, BasicBlock, Function, FunctionData, Program, Value};

#[derive(Default)]
pub struct Environment {
  pub table: SymbolTable,
  pub ctx: Context,
}

#[derive(Debug, Clone)]
pub struct LoopInfo {
  pub continue_bb: BasicBlock,
  pub break_bb: BasicBlock,
}

#[derive(Default)]
pub struct Context {
  pub program: Program,
  pub func: Option<Function>,
  pub block: Option<BasicBlock>,
  pub loop_stack: Vec<LoopInfo>,

  pub terminated_bbs: HashSet<BasicBlock>,
}

impl Context {
  pub fn set_func(&mut self, func: Function) {
    self.func = Some(func);
  }

  pub fn set_block(&mut self, bb: BasicBlock) {
    self.block = Some(bb);
  }

  pub fn clear_block(&mut self) {
    self.block = None;
  }

  pub fn current_open_block(&mut self) -> Option<BasicBlock> {
    if let Some(bb) = self.block {
      if !self.is_block_terminated(bb) {
        return Some(bb);
      }
    }
    None
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

  pub fn mark_block_terminated(&mut self, bb: BasicBlock) {
    self.terminated_bbs.insert(bb);
  }

  pub fn is_block_terminated(&self, bb: BasicBlock) -> bool {
    self.terminated_bbs.contains(&bb)
  }

  pub fn push_loop(&mut self, continue_bb: BasicBlock, break_bb: BasicBlock) {
    self.loop_stack.push(LoopInfo { continue_bb, break_bb });
  }

  pub fn pop_loop(&mut self) -> Option<LoopInfo> {
    self.loop_stack.pop()
  }

  pub fn current_loop(&self) -> Option<&LoopInfo> {
    self.loop_stack.last()
  }
}