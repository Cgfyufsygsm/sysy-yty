use koopa::ir::{TypeKind, Value, ValueKind};

use crate::backend::{env::Environment, reg::RegGuard};

pub fn sp_off(offset: i32) -> String {
  format!("{}(sp)", offset)
}

pub fn addi(dest: &str, src: &str, imm: i32) -> String {
  if imm <= 2047 && imm >= -2048 {
    format!("  addi  {}, {}, {}\n", dest, src, imm)
  } else {
    let tmp_reg = RegGuard::new().expect("No register available");
    let asm = format!("  li    {}, {}\n  add   {}, {}, {}\n", tmp_reg, imm, dest, src, tmp_reg);
    asm
  }
}

/// 计算 rs1 寄存器的值与 imm12 相加的结果作为访存地址, 将 rs2 寄存器的值 (32-bit) 存入内存.
/// sw rs2, imm12(rs1)
pub fn sw(rs2: &str, rs1: &str, offset: i32) -> String {
  if offset <= 2047 && offset >= -2048 {
    format!("  sw    {}, {}({})\n", rs2, offset, rs1)
  } else {
    let tmp_reg = RegGuard::new().expect("No register available");
    let mut asm = String::new();
    asm.push_str(&format!("  li   {}, {}\n", tmp_reg, offset));
    asm.push_str(&format!("  add  {}, {}, {}\n", tmp_reg, tmp_reg, rs1));
    asm.push_str(&format!("  sw    {}, 0({})\n", rs2, tmp_reg));
    asm
  }
}

/// 计算 rd 寄存器的值与 imm12 相加的结果作为访存地址, 从内存中读取 32-bit 的数据, 存入 rs 寄存器.
/// lw rs, imm12(rd)
pub fn lw(rs: &str, rd: &str, offset: i32) -> String {
  if offset <= 2047 && offset >= -2048 {
    format!("  lw    {}, {}({})\n", rs, offset, rd)
  } else {
    let tmp_reg = RegGuard::new().expect("No register available");
    let mut asm = String::new();
    asm.push_str(&format!("  li    {}, {}\n", tmp_reg, offset));
    asm.push_str(&format!("  add   {}, {}, {}\n", tmp_reg, tmp_reg, rd));
    asm.push_str(&format!("  lw    {}, 0({})\n", rs, tmp_reg));
    asm
  }
}

// ===== 工具函数 =====
/// 把任意 Value 装入一个寄存器；返回 (前置汇编, 寄存器名)
pub fn load_operand_to_reg(
  env: &Environment,
  val: Value,
  reg: &mut String,        // 供立即数或栈值使用的临时寄存器
) -> String {

  // 处理如果是函数参数
  if let Some(param_reg) = env.frame_layout().get_param_reg(&val) {
    *reg = param_reg.clone();
    return String::new();
  }

  if val.is_global() {
    let mut asm = env.load_global_addr(val, &reg);
    asm.push_str(&format!("  lw    {}, 0({})\n", reg, reg));
    return asm;
  }

  let data = env.func_data().dfg().value(val);
  match data.kind() {
    ValueKind::Integer(i) => {
      // 立即数
      let asm = format!("  li    {}, {}\n", reg, i.value());
      asm
    }
    _ => {
      // 栈上的局部变量
      let off = env.get_offset(&val);
      lw(reg, "sp", off)
    }
  }
}

/// 计算 ty 类型占用的字节数
/// 如果 is_ptr 为 true，则表示传入的是待计算类型的指针
pub(in crate::backend) fn calculate_size(ty: TypeKind, is_ptr: bool) -> usize {
  match ty {
    TypeKind::Int32 => 4,
    TypeKind::Array(base_ty, len) => calculate_size(base_ty.kind().clone(), false) * len,
    TypeKind::Pointer(base_ty) => {
      if is_ptr {
        calculate_size(base_ty.kind().clone(), false)
      } else {
        4
      }
    }, // Assuming 32-bit pointers
    _ => panic!("Unsupported type for size calculation: {}", ty),
  }
}