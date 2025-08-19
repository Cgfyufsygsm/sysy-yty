use std::collections::HashMap;
use koopa::ir::{Value, FunctionData, ValueKind};
use crate::backend::util::sp_off;

pub struct FrameLayout {
  size: i32,
  offsets: HashMap<Value, i32>,
  param_regs: HashMap<Value, String>,
  ra_saved: bool,
  outgoing_args_size: i32,
}

impl FrameLayout {
  pub fn size(&self) -> i32 {
    self.size
  }

  pub fn get_offset(&self, val: &Value) -> i32 {
    *self.offsets.get(val).expect("value not in frame layout") + self.outgoing_args_size
  }

  pub fn generate_prologue(&self) -> String {
    let mut asm = String::new();
    if self.size() <= 2047 {
      asm.push_str(&format!("  addi  sp, sp, -{}\n", self.size()));
    } else {
      asm.push_str(&format!("  li    t0, {}\n", -self.size()));
      asm.push_str("  add   sp, sp, t0\n");
    }

    if self.ra_saved {
      asm.push_str(&format!("  sw    ra, {}\n", sp_off(self.size - 4)));
    }
    asm
  }

  pub fn generate_epilogue(&self, func_name: &str) -> String {
    let mut asm = String::new();
    asm.push_str(&format!("{}_ret:\n", func_name));

    if self.ra_saved {
      asm.push_str(&format!("  lw    ra, {}\n", sp_off(self.size - 4)));
    }

    if self.size() <= 2047 {
      asm.push_str(&format!("  addi  sp, sp, {}\n", self.size()));
    } else {
      asm.push_str(&format!("  li    t0, {}\n", self.size()));
      asm.push_str("  add   sp, sp, t0\n");
    }
    asm.push_str("  ret\n");
    asm
  }

  pub fn get_param_reg(&self, param: &Value) -> Option<&String> {
    self.param_regs.get(param)
  }
}

pub fn layout_frame(func: &FunctionData) -> FrameLayout {
  let mut offset = 0i32;
  let mut offsets = HashMap::new();
  let mut param_regs = HashMap::new();

  let mut max_extra_args = 0i32;
  let mut ra_needed = false;

  // 先扫指令
  for (_bb, node) in func.layout().bbs() {
    for &inst in node.insts().keys() {
      let data = func.dfg().value(inst);

      if let ValueKind::Call(call) = data.kind() {
        ra_needed = true;
        let len = call.args().len() as i32;
        if len > 8 {
          let extra = len - 8;
          if extra > max_extra_args {
            max_extra_args = extra;
          }
        }
      }

      // alloc 占 4 字节
      if let ValueKind::Alloc(_) = data.kind() {
        offsets.insert(inst, offset);
        offset += 4;
      }
      // 其余有返回值的指令
      else if !data.ty().is_unit() {
        offsets.insert(inst, offset);
        offset += 4;
      }
    }
  }

  let a = max_extra_args * 4;
  let r = if ra_needed { 4 } else { 0 };
  let s = offset;
  let total = a + r + s;
  // 向上对齐 16
  let aligned = (total + 15) & !15;

  for (i, &param) in func.params().iter().enumerate() {
    if i < 8 {
      param_regs.insert(param, format!("a{}", i));
    } else {
      offsets.insert(param, aligned + (i as i32 - 8) * 4);
    }
  }


  FrameLayout { size: aligned, offsets, param_regs, ra_saved: ra_needed, outgoing_args_size: a }
}