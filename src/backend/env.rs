use koopa::ir::{BasicBlock, FunctionData, Value};
use crate::backend::frame::FrameLayout;

pub struct Environment<'a> {
  func_data: &'a FunctionData,
  frame_layout: &'a FrameLayout,
  inst: Value,
}

impl<'a> Environment<'a> {
  pub fn new(func_data: &'a FunctionData, frame_layout: &'a FrameLayout, inst: Value) -> Self {
    Environment {
      func_data,
      frame_layout,
      inst,
    }
  }

  pub fn func_data(&self) -> &FunctionData {
    self.func_data
  }

  pub fn frame_layout(&self) -> &FrameLayout {
    self.frame_layout
  }

  pub fn inst(&self) -> Value {
    self.inst
  }

  pub fn get_self_offset(&self) -> i32 {
    self.frame_layout.get_offset(&self.inst)
  }

  pub fn get_offset(&self, val: &Value) -> i32 {
    self.frame_layout.get_offset(val)
  }

  pub fn func_name(&self) -> &str {
    self.func_data.name().trim_start_matches('@')
  }

  pub fn get_bb_name(&self, bb: BasicBlock) -> &str {
    self.func_data.dfg().bb(bb)
    .name().as_ref()
    .expect("Basic block without name")
    .trim_start_matches(&['@', '%'][..])
  }
}