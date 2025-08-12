use std::collections::HashMap;

use koopa::ir::{entities::ValueData, values::{Return, Binary, BinaryOp}, FunctionData, Program, Value, ValueKind};

pub(crate) trait GenerateAsm {
  fn generate_asm(&self) -> String;
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
    let mut asm = String::new();
    let func_name = self.name().strip_prefix("@").unwrap_or(self.name());
    asm.push_str(&format!("  .globl {}\n", func_name));
    asm.push_str(&format!("{}:\n", func_name));

    let mut reg_map: HashMap<Value, String> = HashMap::new();
    let mut next_t: usize = 0;

    for (_bb, node) in self.layout().bbs() {
      for &inst in node.insts().keys() {
        // 为当前 instruction 分配目标寄存器
        let dest = format!("t{}", next_t);
        next_t += 1;
        reg_map.insert(inst, dest.clone());

        let value_data = self.dfg().value(inst);
        asm.push_str(&value_data.generate_asm(self, inst, &mut reg_map, &mut next_t));
      }
    }
    asm
  }
}

trait GenerateValueAsm {
  fn generate_asm(
    &self,
    func_data: &FunctionData,
    inst: Value,
    reg_map: &mut HashMap<Value, String>,
    next_t: &mut usize,
  ) -> String;
}  

impl GenerateValueAsm for ValueData {
  fn generate_asm(
    &self,
    func_data: &FunctionData,
    inst: Value,
    reg_map: &mut HashMap<Value, String>,
    next_t: &mut usize,
  ) -> String {
    let mut asm = String::new();
    match self.kind() {
      ValueKind::Return(ret) => {
        // value.
        asm.push_str(&ret.generate_asm(func_data, inst, reg_map, next_t));
      }
      ValueKind::Binary(bin) => {
        asm.push_str(&bin.generate_asm(func_data, inst, reg_map, next_t));
      }
      default => {
        // Handle other value kinds if necessary
        asm.push_str(&format!("  ; Unhandled value kind: {:?}\n", default));
      }
    }
    asm
  }
}

impl GenerateValueAsm for Return {
  fn generate_asm(
      &self,
      func_data: &FunctionData,
      _inst: Value,
      reg_map: &mut HashMap<Value, String>,
      _next_t: &mut usize,
    ) -> String {
    let mut asm = String::new();
    if let Some(v) = self.value() {
      let vk = func_data.dfg().value(v).kind();
      match vk {
        ValueKind::Integer(intv) => {
          asm.push_str(&format!("  li    a0, {}\n", intv.value()));
        }
        _ => {
          if let Some(r) = reg_map.get(&v) {
            asm.push_str(&format!("  mv    a0, {}\n", r));
          } else {
            asm.push_str(&format!("  ; WARNING: missing mapping for return value\n"));
          } 
        }
      }
    }else {
      // ret 没有值
      asm.push_str("  ; ret void\n");
    }
    asm.push_str("  ret\n");
    asm
  }
}

fn operand_to_reg(
  func_data: &FunctionData,
  operand: &Value,
  reg_map: &mut HashMap<Value, String>,
  next_t: &mut usize,
) -> (String, String) {
  let kind = func_data.dfg().value(*operand).kind();
  match kind {
    ValueKind::Integer(i) => {
      let imm = i.value();
      if imm == 0 {
        ("".to_string(), "x0".to_string())
      } else {
        let reg = format!("t{}", *next_t);
        *next_t += 1;
        let asm = format!("  li    {}, {}\n", reg, imm);
        (asm, reg)
      }
    }
    _ => {
      if let Some(r) = reg_map.get(&operand) {
        ("".to_string(), r.clone())
      } else {
        let placeholder = format!("t{}", *next_t);
        *next_t += 1;
        (
            format!("  ; WARNING: missing mapping for operand, using {}\n", placeholder),
            placeholder,
        )
      }
    }
  }
}

impl GenerateValueAsm for Binary {
  /// 根据 Binary 实例生成 RISC-V 汇编
  fn generate_asm(
    &self,
    func_data: &FunctionData,
    result_value: Value,
    reg_map: &mut HashMap<Value, String>,
    next_t: &mut usize,
  ) -> String {
    let mut asm = String::new();
    let dest = reg_map.get(&result_value).expect("dest reg should exist").clone();

    let lhs = self.lhs();
    let rhs = self.rhs();

    // 为左右操作数准备寄存器（并可能产生装载 immediate 的 asm）
    let (lhs_preload, lhs_reg) = operand_to_reg(func_data, &lhs, reg_map, next_t);
    let (rhs_preload, rhs_reg) = operand_to_reg(func_data, &rhs, reg_map, next_t);

    asm.push_str(&lhs_preload);
    asm.push_str(&rhs_preload);

    use BinaryOp::*;
    match self.op() {
      // 算术与位运算：先把 lhs 拷贝到 dest，再用 dest = dest op rhs
      Add | Sub | Mul | Div | Mod | And | Or | Xor | Shl | Shr | Sar => {
        // mv dest, lhs_reg
        asm.push_str(&format!("  mv    {}, {}\n", dest, lhs_reg));

        let op_instr = match self.op() {
          Add => "add",
          Sub => "sub",
          Mul => "mul",
          Div => "div",
          Mod => "rem",
          And => "and",
          Or  => "or",
          Xor => "xor",
          Shl => "sll",
          Shr => "srl",
          Sar => "sra",
          _ => unreachable!(),
        };

        asm.push_str(&format!("  {}    {}, {}, {}\n", op_instr, dest, dest, rhs_reg));
      }

      // 比较/关系运算：分别处理
      Eq => {
        // xor tmp = lhs ^ rhs ; seqz dest, tmp
        asm.push_str(&format!("  xor   {}, {}, {}\n", dest, lhs_reg, rhs_reg));
        asm.push_str(&format!("  seqz  {}, {}\n", dest, dest));
      }
      BinaryOp::NotEq => {
        // xor tmp ; seqz tmp ; xori dest, tmp, 1
        asm.push_str(&format!("  xor   {}, {}, {}\n", dest, lhs_reg, rhs_reg));
        asm.push_str(&format!("  seqz  {}, {}\n", dest, dest));
        asm.push_str(&format!("  xori  {}, {}, 1\n", dest, dest));
      }
      BinaryOp::Lt => {
        // slt dest, lhs, rhs
        asm.push_str(&format!("  slt   {}, {}, {}\n", dest, lhs_reg, rhs_reg));
      }
      BinaryOp::Gt => {
        // slt dest, rhs, lhs  (left > right  <=>  rhs < lhs)
        asm.push_str(&format!("  slt   {}, {}, {}\n", dest, rhs_reg, lhs_reg));
      }
      BinaryOp::Le => {
        // left <= right  <=>  not (left > right)
        // slt tmp, rhs, lhs  ; seqz dest, tmp
        asm.push_str(&format!("  slt   {}, {}, {}\n", dest, rhs_reg, lhs_reg));
        asm.push_str(&format!("  seqz  {}, {}\n", dest, dest));
      }
      BinaryOp::Ge => {
        // left >= right  <=>  not (left < right)
        // slt tmp, lhs, rhs ; seqz dest, tmp
        asm.push_str(&format!("  slt   {}, {}, {}\n", dest, lhs_reg, rhs_reg));
        asm.push_str(&format!("  seqz  {}, {}\n", dest, dest));
      }
    }

    asm
  }
}
