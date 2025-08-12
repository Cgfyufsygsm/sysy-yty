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

struct RegPool {
  free: Vec<&'static str>,
}

impl RegPool {
  fn new() -> Self {
    RegPool {
      free: vec!["t0", "t1", "t2", "t3", "t4", "t5", "t6", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7"],
    }
  }

  fn alloc(&mut self) -> Option<&'static str> {
    self.free.pop()
  }

  fn release(&mut self, reg: &'static str) {
    if !self.free.contains(&reg) {
      self.free.push(reg);
    }
  }
}

fn compute_live_until(func: &FunctionData) -> HashMap<Value, usize> {
  let mut live_until = HashMap::new();
  let mut index = 0usize;
  for (_bb, node) in func.layout().bbs() {
    for &inst in node.insts().keys() {
      let value_data = func.dfg().value(inst);
      match value_data.kind() {
        ValueKind::Binary(bin) => {
          for op in &[bin.lhs(), bin.rhs()] {
            *live_until.entry(*op).or_insert(index) = index;
          }
        }
        ValueKind::Return(ret) => {
          if let Some(v) = ret.value() {
            *live_until.entry(v).or_insert(index) = index;
          }
        }
        _ => {}
      }
      index += 1;
    }
  }
  live_until
}

impl GenerateAsm for FunctionData {
  fn generate_asm(&self) -> String {
    let mut asm = String::new();
    let func_name = self.name().strip_prefix("@").unwrap_or(self.name());
    asm.push_str(&format!("  .globl {}\n", func_name));
    asm.push_str(&format!("{}:\n", func_name));

    let live_until = compute_live_until(self);
    let mut reg_map: HashMap<Value, &'static str> = HashMap::new();
    let mut pool = RegPool::new();
    let mut index = 0usize;

    for (_bb, node) in self.layout().bbs() {
      for &inst in node.insts().keys() {
        // release unused registers
        let mut to_release = Vec::new();
        for (&val, &reg) in reg_map.iter() {
          if live_until.get(&val).copied().unwrap_or(0) < index {
            to_release.push(reg);
          }
        }
        for reg in to_release {
          reg_map.retain(|_, v| *v != reg);
          pool.release(reg);
        }
        let reg = pool.alloc().expect("register pool exhausted");
        reg_map.insert(inst, reg);

        let value_data = self.dfg().value(inst);
        asm.push_str(&value_data.generate_asm(self, inst, &mut reg_map, &mut pool));
        index += 1;
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
    reg_map: &mut HashMap<Value, &'static str>,
    pool: &mut RegPool,
  ) -> String;
}  

impl GenerateValueAsm for ValueData {
  fn generate_asm(
    &self,
    func_data: &FunctionData,
    inst: Value,
    reg_map: &mut HashMap<Value, &'static str>,
    pool: &mut RegPool,
  ) -> String {
    let mut asm = String::new();
    match self.kind() {
      ValueKind::Return(ret) => {
        // value.
        asm.push_str(&ret.generate_asm(func_data, inst, reg_map, pool));
      }
      ValueKind::Binary(bin) => {
        asm.push_str(&bin.generate_asm(func_data, inst, reg_map, pool));
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
      reg_map: &mut HashMap<Value, &'static str>,
      _pool: &mut RegPool,
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
  reg_map: &mut HashMap<Value, &'static str>,
  pool: &mut RegPool,
) -> (String, &'static str) {
  let kind = func_data.dfg().value(*operand).kind();
  match kind {
    ValueKind::Integer(i) => {
      let imm = i.value();
      if imm == 0 {
        ("".to_string(), "x0")
      } else {
        let reg = pool.alloc().expect("register pool exhausted");
        let asm = format!("  li    {}, {}\n", reg, imm);
        (asm, reg)
      }
    }
    _ => {
      let reg = *reg_map.get(operand).expect("operand should have a mapped register");
      (String::new(), reg)
    }
  }
}

impl GenerateValueAsm for Binary {
  /// 根据 Binary 实例生成 RISC-V 汇编
  fn generate_asm(
    &self,
    func_data: &FunctionData,
    result_value: Value,
    reg_map: &mut HashMap<Value, &'static str>,
    pool: &mut RegPool,
  ) -> String {
    let mut asm = String::new();
    let dest = *reg_map.get(&result_value).expect("dest reg should exist");

    let lhs = self.lhs();
    let rhs = self.rhs();

    // 为左右操作数准备寄存器（并可能产生装载 immediate 的 asm）
    let (lhs_preload, lhs_reg) = operand_to_reg(func_data, &lhs, reg_map, pool);
    let (rhs_preload, rhs_reg) = operand_to_reg(func_data, &rhs, reg_map, pool);

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
