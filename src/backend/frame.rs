use std::collections::HashMap;
use koopa::ir::{FunctionData, Value, ValueKind};
use crate::backend::util::{addi, calculate_size, lw, sw};

struct ValueOffset {
  offset: i32,
  is_ptr: bool,
}

pub struct FrameLayout {
  size: i32,
  offsets: HashMap<Value, ValueOffset>,
  param_regs: HashMap<Value, String>,
  ra_saved: bool,
  outgoing_args_size: i32,
}

impl FrameLayout {
  pub fn size(&self) -> i32 {
    self.size
  }

  pub fn get_offset(&self, val: &Value) -> i32 {
    self.offsets.get(val).expect("value not in frame layout").offset + self.outgoing_args_size
  }

  // getelemptr 和 getptr 的结果是指针类型
  // 必须要在 store 和 load 的时候进行特殊处理
  // 先把指针的值加载到寄存器里，再通过寄存器访存
  pub fn is_ptr(&self, val: &Value) -> bool {
    if val.is_global() {
      return false;
    }
    self.offsets.get(val).expect("value not in frame layout").is_ptr
  }

  pub fn generate_prologue(&self) -> String {
    let mut asm = String::new();
    asm.push_str(&addi("sp", "sp", -self.size()));

    if self.ra_saved {
      asm.push_str(&sw("ra", "sp", self.size - 4));
    }
    asm
  }

  pub fn generate_epilogue(&self, func_name: &str) -> String {
    let mut asm = String::new();
    asm.push_str(&format!("{}_ret:\n", func_name));

    if self.ra_saved {
      asm.push_str(&lw("ra", "sp", self.size - 4));
    }

    asm.push_str(&addi("sp", "sp", self.size()));
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
        offsets.insert(inst, ValueOffset { offset, is_ptr: false });
        offset += calculate_size(data.ty().kind().clone(), true) as i32;
      } else if let ValueKind::GetElemPtr(_) = data.kind() {
        offsets.insert(inst, ValueOffset { offset, is_ptr: true });
        offset += 4; // GetElemPtr 结果是指针，固定占用 4 字节
      } else if let ValueKind::GetPtr(_) = data.kind() {
        offsets.insert(inst, ValueOffset { offset, is_ptr: true });
        offset += 4; // GetPtr 结果是指针，固定占用 4 字节
      } else if !data.ty().is_unit() {
        offsets.insert(inst, ValueOffset { offset, is_ptr: false });
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
      // TODO 处理数组参数
      offsets.insert(param, ValueOffset { offset: aligned + (i as i32 - 8) * 4, is_ptr: false });
      // offsets.insert(param, aligned + (i as i32 - 8) * 4);
    }
  }


  FrameLayout { size: aligned, offsets, param_regs, ra_saved: ra_needed, outgoing_args_size: a }
}