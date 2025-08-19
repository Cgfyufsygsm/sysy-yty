use koopa::ir::{Value, ValueKind};

use crate::backend::env::Environment;

pub fn sp_off(offset: i32) -> String {
  format!("{}(sp)", offset)
}

// ===== 工具函数 =====
/// 把任意 Value 装入一个寄存器；返回 (前置汇编, 寄存器名)
pub fn load_operand_to_reg(
  env: &Environment,
  val: Value,
  scratch: String,        // 供立即数或栈值使用的临时寄存器
) -> (String, String) {

  // 处理如果是函数参数
  if let Some(reg) = env.frame_layout().get_param_reg(&val) {
    return (String::new(), reg.clone());
  }

  if val.is_global() {
    let mut asm = env.load_global_addr(val, &scratch);
    asm.push_str(&format!("  lw    {}, 0({})\n", scratch, scratch));
    return (asm, scratch);
  }

  let data = env.func_data().dfg().value(val);
  match data.kind() {
    ValueKind::Integer(i) => {
      // 立即数
      let asm = format!("  li    {}, {}\n", scratch, i.value());
      (asm, scratch)
    }
    _ => {
      // 栈上的局部变量
      let off = env.get_offset(&val);
      let asm = format!("  lw    {}, {}(sp)\n", scratch, off);
      (asm, scratch)
    }
  }
}