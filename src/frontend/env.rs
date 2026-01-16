use std::collections::HashSet;

use crate::frontend::symbol::SymbolTable;
use koopa::ir::{builder::{BasicBlockBuilder, GlobalBuilder, GlobalInstBuilder, LocalBuilder, LocalInstBuilder, ValueBuilder}, BasicBlock, BinaryOp, Function, FunctionData, Program, Type, Value};

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

  pub fn clear_func(&mut self) {
    self.func = None;
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

  pub fn is_global(&self) -> bool {
    self.func.is_none()
  }

  fn local_builder(&mut self) -> LocalBuilder<'_> {
    self.func_data().dfg_mut().new_value()
  }

  fn global_builder(&mut self) -> GlobalBuilder<'_> {
    self.program.new_value()
  }

  pub fn local_integer(&mut self, value: i32) -> Value {
    self.local_builder().integer(value)
  }

  pub fn global_integer(&mut self, value: i32) -> Value {
    self.global_builder().integer(value)
  }

  pub fn global_alloc(&mut self, init: Value) -> Value {
    self.global_builder().global_alloc(init)
  }

  pub fn local_alloc(&mut self, ty: Type) -> Value {
    self.local_builder().alloc(ty)
  }

  pub fn local_store(&mut self, value: Value, dest: Value) -> Value {
    self.local_builder().store(value, dest)
  }

  pub fn global_zero_init(&mut self, ty: Type) -> Value {
    self.global_builder().zero_init(ty)
  }

  pub fn global_aggregate(&mut self, elems: Vec<Value>) -> Value {
    self.global_builder().aggregate(elems)
  }

  pub fn local_load(&mut self, src: Value) -> Value {
    self.local_builder().load(src)
  }

  pub fn ret(&mut self, value: Option<Value>) -> Value {
    self.local_builder().ret(value)
  }

  pub fn branch(&mut self, cond: Value, true_bb: BasicBlock, false_bb: BasicBlock) -> Value {
    self.local_builder().branch(cond, true_bb, false_bb)
  }

  pub fn jump(&mut self, target: BasicBlock) -> Value {
    self.local_builder().jump(target)
  }

  pub fn get_elem_ptr(&mut self, src: Value, index: Value) -> Value {
    self.local_builder().get_elem_ptr(src, index)
  }

  pub fn get_ptr(&mut self, src: Value, index: Value) -> Value {
    self.local_builder().get_ptr(src, index)
  }

  pub fn binary(&mut self, op: BinaryOp, lhs: Value, rhs: Value) -> Value {
    self.local_builder().binary(op, lhs, rhs)
  }

  pub fn call(&mut self, callee: Function, args: Vec<Value>) -> Value {
    self.local_builder().call(callee, args)
  }

  pub fn alloc_and_store(&mut self, value: Value, ty: Type) -> Value {
    let alloc = self.local_builder().alloc(ty);
    let store = self.local_builder().store(value, alloc);
    self.add_inst(alloc);
    self.add_inst(store);
    alloc
  }

  pub fn set_global_name(&mut self, value: Value, name: String) {
    self.program.set_value_name(value, Some(format!("@{}", name)));
  }

  pub fn set_value_name(&mut self, value: Value, name: String) {
    self.func_data().dfg_mut().set_value_name(value, Some(format!("%{}", name)));
  }

  pub fn get_value_ty(&mut self, value: Value) -> Type {
    if value.is_global() {
      self.program.borrow_value(value).ty().clone()
    } else {
      self.func_data().dfg().value(value).ty().clone()
    }
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
