use std::default;

use koopa::ir::{entities::ValueData, values::Return, FunctionData, Program, ValueKind};

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

    for (bb, node) in self.layout().bbs() {
      for &inst in node.insts().keys() {
        let value_data = self.dfg().value(inst);
        asm.push_str(&value_data.generate_asm(self));
      }
    }
    asm
  }
}

trait GenerateAsm2 {
  fn generate_asm(&self, func_data: &FunctionData) -> String;
}  

impl GenerateAsm2 for ValueData {
  fn generate_asm(&self, func_data: &FunctionData) -> String {
    let mut asm = String::new();
    match self.kind() {
      ValueKind::Return(ret) => {
        // value.
        asm.push_str(&ret.generate_asm(func_data));
      }
      default => {
        // Handle other value kinds if necessary
        asm.push_str(&format!("  ; Unhandled value kind: {:?}\n", default));
      }
    }
    asm
  }
}

impl GenerateAsm2 for Return {
  fn generate_asm(&self, func_data: &FunctionData) -> String {
    let mut asm = String::new();
    if let Some(value) = self.value() {
      let value_data = func_data.dfg().value(value);
      match value_data.kind() {
        ValueKind::Integer(ret_val) => {
          asm.push_str(&format!("  li a0, {}\n", ret_val.value()));
        }
        default => {
          asm.push_str(&format!("  ; Unhandled return value kind: {:?}\n", default));
        }
      }
    }
    asm.push_str("  ret");
    asm
  }
}