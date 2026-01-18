use std::{collections::HashMap};

use koopa::ir::{BasicBlock, Function, FunctionData, Program, Type, Value};
use crate::backend::frame::FrameLayout;
use crate::backend::mir::{Reg, VRegInfo};

pub struct Environment<'a> {
  program: &'a Program,
  func: Option<Function>,
  frame_layout: Option<FrameLayout>,
  inst: Option<Value>,
  global_table: &'a HashMap<Value, String>,
  value_regs: HashMap<Value, Reg>,
  vreg_info: VRegInfo,
}

impl<'a> Environment<'a> {
  pub fn new(program: &'a Program, global_table: &'a HashMap<Value, String>) -> Self {
    Self {
      program,
      func: None,
      frame_layout: None,
      inst: None,
      global_table,
      value_regs: HashMap::new(),
      vreg_info: VRegInfo::default(),
    }
  }

  pub fn set_func(&mut self, func: Function) {
    self.func = Some(func);
    self.value_regs.clear();
    self.vreg_info = VRegInfo::default();
  }

  pub fn set_frame_layout(&mut self, layout: FrameLayout) {
    self.frame_layout = Some(layout);
  }

  pub fn set_inst(&mut self, inst: Value) {
    self.inst = Some(inst);
  }

  pub fn clear_inst(&mut self) {
    self.inst = None;
  }

  pub fn clear_frame_layout(&mut self) {
    self.frame_layout = None;
  }

  pub fn clear_func(&mut self) {
    self.func = None;
  }

  pub fn alloc_value_reg(&mut self, value: Value) -> Reg {
    if let Some(reg) = self.value_regs.get(&value) {
      return reg.clone();
    }
    let reg = self.alloc_vreg();
    if let Reg::Virt(id) = reg {
      if let Some(off) = self.frame_layout().try_get_offset(&value) {
        self.vreg_info.spill_offsets.insert(id, off);
      }
    }
    self.value_regs.insert(value, reg.clone());
    reg
  }

  pub fn alloc_temp_reg(&mut self) -> Reg {
    self.alloc_vreg()
  }

  pub fn get_value_reg(&self, value: Value) -> Option<Reg> {
    self.value_regs.get(&value).cloned()
  }

  pub fn get_self_reg(&mut self) -> Reg {
    let inst = *self.inst();
    self.alloc_value_reg(inst)
  }

  pub fn take_vreg_info(&mut self) -> VRegInfo {
    std::mem::take(&mut self.vreg_info)
  }

  fn alloc_vreg(&mut self) -> Reg {
    let id = self.vreg_info.next_vreg;
    self.vreg_info.next_vreg += 1;
    Reg::Virt(id)
  }

  pub fn func_data(&self) -> &FunctionData {
    self.program.func(self.func.expect("No function set"))
  }

  pub fn frame_layout(&self) -> &FrameLayout {
    self.frame_layout.as_ref().expect("frame layout not set")
  }

  pub fn inst(&self) -> &Value {
    self.inst.as_ref().expect("No instruction set")
  }

  pub fn get_self_offset(&self) -> i32 {
    self.frame_layout().get_offset(&self.inst())
  }

  pub fn get_offset(&self, val: &Value) -> i32 {
    self.frame_layout().get_offset(val)
  }

  pub fn cur_func_name(&self) -> &str {
    self.func_data().name().trim_start_matches('@')
  }

  pub fn get_func_data(&self, func: Function) -> &FunctionData {
    self.program.func(func)
  }

  pub fn get_value_ty(&self, value: Value) -> Type {
    if value.is_global() {
      self.program.borrow_value(value).ty().clone()
    } else {
      self.func_data().dfg().value(value).ty().clone()
    }
  }

  pub fn get_bb_name(&self, bb: BasicBlock) -> &str {
    self.func_data().dfg().bb(bb)
    .name().as_ref()
    .expect("Basic block without name")
    .trim_start_matches(&['@', '%'][..])
  }

  pub fn get_global_name(&self, value: Value) -> Option<&String> {
    self.global_table.get(&value)
  }

  pub fn load_global_addr(&self, value: Value, reg: &str) -> String {
    let global_name = self.get_global_name(value).expect("Global variable not found");
    format!("  la    {}, {}\n", reg, global_name)
  }
}
