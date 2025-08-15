use koopa::ir::{entities::ValueData, values::{Binary, BinaryOp, Branch, Integer, Jump, Load, Return, Store, Call}, FunctionData, Program, ValueKind};
use crate::backend::{env::Environment, frame::{layout_frame}, util::{sp_off, load_operand_to_reg}};

pub(crate) trait GenerateAsm {
  fn generate_asm(&self) -> String;
}

static SYSY_LIB_FUNCTIONS: [&str; 8] = [
  "getint", "getch", "getarray", "putint", "putch", "putarray", "starttime", "stoptime",
];

impl GenerateAsm for Program {
  fn generate_asm(&self) -> String {
    let mut asm = String::from("  .text\n");

    for &func in self.func_layout() {
      let func_data = self.func(func);
      if func_data.name().starts_with('@') {
        let func_name = func_data.name().trim_start_matches('@');
        if SYSY_LIB_FUNCTIONS.contains(&func_name) {
          continue; // Skip sysy library functions
        }
      }
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

    asm.push_str(&layout.generate_prologue());

    for (bb, node) in self.layout().bbs() {
      let bb_name = self.dfg().bb(*bb).name();
      match bb_name {
        Some(name) => asm.push_str(&format!("{}:\n", name.trim_start_matches(&['@', '%'][..]))),
        None => unreachable!("Basic block without name"),
      }
      for &inst in node.insts().keys() {
        let value_data = self.dfg().value(inst);
        let env = Environment::new(self, &layout, inst);
        asm.push_str(&value_data.generate_asm(&env));
      }
    }

    asm.push_str(&layout.generate_epilogue(func_name));

    asm
  }
}

trait GenerateStackAsm {
  fn generate_asm(
    &self,
    env: &Environment,
  ) -> String;
}

impl GenerateStackAsm for ValueData {
  fn generate_asm(
    &self,
    env: &Environment,
  ) -> String {
    let mut asm = String::new();

    asm.push_str(&match self.kind() {
      ValueKind::Integer(i) => i.generate_asm(env),
      ValueKind::Load(load) => load.generate_asm(env),
      ValueKind::Store(store) => store.generate_asm(env),

      ValueKind::Return(ret) => ret.generate_asm(env),
      ValueKind::Binary(bin) => bin.generate_asm(env),
      ValueKind::Branch(br) => br.generate_asm(env),

      ValueKind::Jump(jump) => jump.generate_asm(env),
      
      ValueKind::Alloc(_) => String::new(),

      default => {
        // Handle other value kinds if necessary
        format!("  ; Unhandled value kind: {:?}\n", default)
      }
    });
    asm
  }
}

impl GenerateStackAsm for Integer {
  fn generate_asm(
      &self,
      env: &Environment,
    ) -> String {
    let mut asm = String::new();
    asm.push_str(&format!("  li    t0, {}\n", self.value()));
    asm.push_str(&format!("  sw    t0, {}\n", sp_off(env.get_self_offset())));
    asm
  }
}

impl GenerateStackAsm for Load {
  fn generate_asm(
      &self,
      env: &Environment,
    ) -> String {
    let mut asm = String::new();
    let ptr = self.src();
    let ptr_off = env.get_offset(&ptr);
    asm.push_str(&format!("  lw    t0, {}\n", sp_off(ptr_off)));
    asm.push_str(&format!("  sw    t0, {}\n", sp_off(env.get_self_offset())));
    asm
  }
}

impl GenerateStackAsm for Store {
  fn generate_asm(
      &self,
      env: &Environment,
    ) -> String {
    let mut asm = String::new();
    let value = self.value();
    let ptr = self.dest();
    let (val_asm, val_reg) = load_operand_to_reg(env, value, "t0");
    let ptr_off = env.get_offset(&ptr);
    asm.push_str(&val_asm);
    asm.push_str(&format!("  sw    {}, {}\n", val_reg, sp_off(ptr_off)));
    asm
  }
}

impl GenerateStackAsm for Return {
  fn generate_asm(
    &self,
    env: &Environment,
    ) -> String {
    let mut asm = String::new();
    if let Some(v) = self.value() {
      let vk = env.func_data().dfg().value(v).kind();
      match vk {
        ValueKind::Integer(intv) => {
          asm.push_str(&format!("  li   a0, {}\n", intv.value()));
        }
        _ => {
          let off = env.get_offset(&v);
          asm.push_str(&format!("  lw    a0, {}\n", sp_off(off)));
        }
      }
    }
    asm.push_str(&format!("  j     {}_ret\n", env.func_name()));
    asm
  }
}

impl GenerateStackAsm for Binary {
  /// 根据 Binary 实例生成 RISC-V 汇编
  fn generate_asm(
    &self,
    env: &Environment,
    ) -> String {
    let mut asm = String::new();

    let lhs = self.lhs();
    let rhs = self.rhs();

    let (lhs_asm, lhs_reg) = load_operand_to_reg(env, lhs, "t0");
    let (rhs_asm, rhs_reg) = load_operand_to_reg(env, rhs, "t1");
    let dest_off = env.get_self_offset();

    asm.push_str(&lhs_asm);
    asm.push_str(&rhs_asm);

    use BinaryOp::*;
    let op_str = match self.op() {
      Add => "add", Sub => "sub", Mul => "mul", Div => "div",
      Mod => "rem", And => "and", Or => "or", Xor => "xor",
      Shl => "sll", Shr => "srl", Sar => "sra",
      Eq => "seqz", NotEq => "snez", Lt => "slt", Le => "sle",
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

impl GenerateStackAsm for Branch {
  fn generate_asm(
      &self,
      env: &Environment,
    ) -> String {
    let mut asm = String::new();
    let cond = self.cond();
    let (cond_asm, cond_reg) = load_operand_to_reg(env, cond, "t0");
    asm.push_str(&cond_asm);
    let then_bb = env.get_bb_name(self.true_bb());
    let else_bb = env.get_bb_name(self.false_bb());
    asm.push_str(&format!("  bnez  {}, {}\n", cond_reg, then_bb));
    asm.push_str(&format!("  j     {}\n", else_bb));
    asm
  }
}

impl GenerateStackAsm for Jump {
  fn generate_asm(
      &self,
      env: &Environment,
    ) -> String {
    let mut asm = String::new();
    let target = self.target();
    let target_name = env.get_bb_name(target);
    asm.push_str(&format!("  j     {}\n", target_name));
    asm
  }
}

impl GenerateStackAsm for Call {
  fn generate_asm(
      &self,
      env: &Environment,
    ) -> String {
    let mut asm = String::new();
    let func = self.callee();
    let args = self.args();
    
    /*
     * TODO: 处理函数调用的参数传递
     * prologue 和 epilogue 保存 ra 寄存器
     * 前 8 个参数塞进 a0-a7
     * 剩下的，以及 ra 在栈帧上如何分配，看那张图
     */

    asm
  }
}