use std::{collections::HashMap};

use koopa::{ir::{entities::ValueData, values::{Binary, BinaryOp, Return}, FunctionData, Program, Value, ValueKind}};

pub(crate) trait GenerateAsm {
  fn generate_asm(&self) -> String;
}

struct FrameLayout {
  size: i32,
  offsets: HashMap<Value, i32>,
}

fn layout_frame(func: &FunctionData) -> FrameLayout {
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

impl GenerateAsm for Program {
  fn generate_asm(&self) -> String {
    let mut asm = String::from("  .text\n");

    for &func in self.func_layout() {
      let func_data = self.func(func);
      asm.push_str(&func_data.generate_asm());
    }
    asm
  }
}

impl GenerateAsm for FunctionData {
  fn generate_asm(&self) -> String {
    let layout = layout_frame(self);
    let mut asm = String::new();
    let func_name = self.name().trim_start_matches('@');
    asm.push_str(&format!("  .globl {}\n{}:\n", func_name, func_name));

    // =====prologue=====
    if layout.size <= 2047 {
      asm.push_str(&format!("  addi  sp, sp, -{}\n", layout.size));
    } else {
      asm.push_str(&format!("  li    t0, {}\n", -layout.size));
      asm.push_str("  add   sp, sp, t0\n");
    }

    for (_bb, node) in self.layout().bbs() {
      for &inst in node.insts().keys() {
        // release unused registers
        let value_data = self.dfg().value(inst);
        asm.push_str(&value_data.generate_asm(self, inst, &layout));
      }
    }

    // ===== epilogue =====
    if layout.size <= 2047 {
      asm.push_str(&format!("  addi  sp, sp, {}\n", layout.size));
    } else {
      asm.push_str(&format!("  li    t0, {}\n", layout.size));
      asm.push_str("  add   sp, sp, t0\n");
    }
    asm.push_str("  ret\n");
    asm
  }
}

trait GenerateStackAsm {
  fn generate_asm(
    &self,
    func_data: &FunctionData,
    inst: Value,
    layout: &FrameLayout,
  ) -> String;
}

fn sp_off(offset: i32) -> String {
  format!("{}(sp)", offset)
}

// ===== 工具函数 =====
/// 把任意 Value 装入一个寄存器；返回 (前置汇编, 寄存器名)
fn load_operand_to_reg(
  func: &FunctionData,
  val: Value,
  layout: &FrameLayout,
  scratch: &'static str,        // 供立即数或栈值使用的临时寄存器
) -> (String, &'static str) {
  let data = func.dfg().value(val);
  match data.kind() {
    ValueKind::Integer(i) => {
      // 立即数
      let asm = format!("  li    {}, {}\n", scratch, i.value());
      (asm, scratch)
    }
    _ => {
      // 栈上的值
      let off = *layout.offsets.get(&val).expect("operand not in frame");
      let asm = format!("  lw    {}, {}(sp)\n", scratch, off);
      (asm, scratch)
    }
  }
}

impl GenerateStackAsm for ValueData {
  fn generate_asm(
    &self,
    func_data: &FunctionData,
    inst: Value,
    layout: &FrameLayout,
  ) -> String {
    let mut asm = String::new();
    let offset = layout.offsets.get(&inst);

    match self.kind() {
      ValueKind::Alloc(_) => {}

      ValueKind::Integer(i) => {
        asm.push_str(&format!("  li    t0, {}\n", i.value()));
        asm.push_str(&format!("  sw    t0, {}\n", sp_off(*offset.unwrap())));
      }

      ValueKind::Load(load) => {
        let ptr = load.src();
        let ptr_off = *layout.offsets.get(&ptr).expect("load src not in frame");
        asm.push_str(&format!("  lw    t0, {}\n", sp_off(ptr_off)));
        asm.push_str(&format!("  sw    t0, {}\n", sp_off(*offset.unwrap())));
      }

      ValueKind::Store(store) => {
        let value = store.value();
        let ptr = store.dest();
        let (val_asm, val_reg) = load_operand_to_reg(func_data, value, layout, "t0");
        let ptr_off = *layout.offsets.get(&ptr).expect("store dst not in frame");
        asm.push_str(&val_asm);
        asm.push_str(&format!("  sw    {}, {}\n", val_reg, sp_off(ptr_off)));
      }

      ValueKind::Return(ret) => {
        // value.
        asm.push_str(&ret.generate_asm(func_data, inst, layout));
      }
      ValueKind::Binary(bin) => {
        asm.push_str(&bin.generate_asm(func_data, inst, layout));
      }
      default => {
        // Handle other value kinds if necessary
        asm.push_str(&format!("  ; Unhandled value kind: {:?}\n", default));
      }
    }
    asm
  }
}

impl GenerateStackAsm for Return {
  fn generate_asm(
    &self,
    func_data: &FunctionData,
    _inst: Value,
    layout: &FrameLayout,
  ) -> String {
    let mut asm = String::new();
    if let Some(v) = self.value() {
      let vk = func_data.dfg().value(v).kind();
      match vk {
        ValueKind::Integer(intv) => {
          asm.push_str(&format!("  li   a0, {}\n", intv.value()));
        }
        _ => {
          let off = *layout.offsets.get(&v).expect("return value not in frame");
          asm.push_str(&format!("  lw    a0, {}\n", sp_off(off)));
        }
      }
    } else {
      // ret 没有值
      panic!("unimplemented error")
    }
    asm
  }
}

impl GenerateStackAsm for Binary {
  /// 根据 Binary 实例生成 RISC-V 汇编
  fn generate_asm(
    &self,
    func_data: &FunctionData,
    result_value: Value,
    layout: &FrameLayout,
  ) -> String {
    let mut asm = String::new();

    let lhs = self.lhs();
    let rhs = self.rhs();

    let (lhs_asm, lhs_reg) = load_operand_to_reg(func_data, lhs, layout, "t0");
    let (rhs_asm, rhs_reg) = load_operand_to_reg(func_data, rhs, layout, "t1");
    let dest_off = *layout.offsets.get(&result_value).expect("binary dest not in frame"); 

    asm.push_str(&lhs_asm);
    asm.push_str(&rhs_asm);

    use BinaryOp::*;
    let op_str = match self.op() {
      Add => "add", Sub => "sub", Mul => "mul", Div => "div",
      Mod => "rem", And => "and", Or => "or", Xor => "xor",
      Shl => "sll", Shr => "srl", Sar => "sra",
      Eq => "seq", NotEq => "sne", Lt => "slt", Le => "sle",
      Gt => "sgt", Ge => "sge",
    };

    match self.op() {
      Eq | NotEq => {
        asm.push_str(&format!("  sub   {}, {}, {}\n", lhs_reg, lhs_reg, rhs_reg));
        asm.push_str(&format!("  {}   {}, {}\n", op_str, lhs_reg, lhs_reg));
      }
      _ => {
        asm.push_str(&format!("  {}   {}, {}, {}\n", op_str, lhs_reg, lhs_reg, rhs_reg));
      }
    }
    asm.push_str(&format!("  sw    {}, {}\n", lhs_reg, sp_off(dest_off)));

    asm
  }
}
