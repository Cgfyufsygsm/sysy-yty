use std::collections::HashMap;
use koopa::ir::{Value, FunctionData, ValueKind};

pub struct FrameLayout {
  size: i32,
  offsets: HashMap<Value, i32>,
}

impl FrameLayout {
  pub fn size(&self) -> i32 {
    self.size
  }

  pub fn get_offset(&self, val: &Value) -> i32 {
    *self.offsets.get(val).expect("value not in frame layout")
  }

  pub fn generate_prologue(&self) -> String {
    let mut asm = String::new();
    if self.size() <= 2047 {
      asm.push_str(&format!("  addi  sp, sp, -{}\n", self.size()));
    } else {
      asm.push_str(&format!("  li    t0, {}\n", -self.size()));
      asm.push_str("  add   sp, sp, t0\n");
    }
    asm
  }

  pub fn generate_epilogue(&self, func_name: &str) -> String {
    let mut asm = String::new();
    asm.push_str(&format!("{}_ret:\n", func_name));
    if self.size() <= 2047 {
      asm.push_str(&format!("  addi  sp, sp, {}\n", self.size()));
    } else {
      asm.push_str(&format!("  li    t0, {}\n", self.size()));
      asm.push_str("  add   sp, sp, t0\n");
    }
    asm.push_str("  ret\n");
    asm
  }
}

pub fn layout_frame(func: &FunctionData) -> FrameLayout {
  let mut offset = 0i32;
  let mut offsets = HashMap::new();

  // 先扫指令
  for (_bb, node) in func.layout().bbs() {
    for &inst in node.insts().keys() {
      let data = func.dfg().value(inst);
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
  // 向上对齐 16
  let aligned = (offset + 15) & !15;
  FrameLayout { size: aligned, offsets }
}