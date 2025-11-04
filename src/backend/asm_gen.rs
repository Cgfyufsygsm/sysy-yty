use core::panic;
use std::{collections::HashMap, ops::Deref};

use koopa::ir::{entities::ValueData, values::{Binary, BinaryOp, Branch, Call, GetElemPtr, GetPtr, Integer, Jump, Load, Return, Store}, FunctionData, Program, TypeKind, Value, ValueKind};
use crate::backend::{env::Environment, frame::layout_frame, reg::RegGuard, util::{calculate_size, load_operand_to_reg, lw, addi, sw}};

pub trait GenerateProgAsm {
  fn generate_asm(&self) -> String;
}

trait GenerateAsm {
  fn generate_asm(
    &self,
    env: &mut Environment,
  ) -> String;
}

static SYSY_LIB_FUNCTIONS: [&str; 8] = [
  "getint", "getch", "getarray", "putint", "putch", "putarray", "starttime", "stoptime",
];

fn generate_global_alloc(
  program: &Program,
  value: &Value,
) -> String {
  let mut asm = String::new();
  match program.borrow_value(*value).deref().kind() {
    ValueKind::Integer(int_val) => {
      asm.push_str(&format!("  .word {}\n", int_val.value()));
    }
    ValueKind::ZeroInit(_) => {
      let ty = program.borrow_value(*value).deref().ty().kind().clone();
      let size = calculate_size(ty, false);
      asm.push_str(&format!("  .zero {}\n", size));
    }
    ValueKind::Aggregate(aggregate) => {
      for elem in aggregate.elems() {
        asm.push_str(&generate_global_alloc(program, elem));
      }
    }
    _ => panic!("Expected Integer, ZeroInit or Aggregate for global variable"),
  }
  asm
}

impl GenerateProgAsm for Program {
  fn generate_asm(&self) -> String {
    let mut asm = String::from("  .data\n");

    let mut global_table = HashMap::new();
    for &value in self.inst_layout() {
      let var_data = self.borrow_value(value);
      let var_name = var_data.name().as_ref().unwrap().trim_start_matches('@');

      global_table.insert(value, var_name.to_string());
      asm.push_str(&format!("  .globl {}\n{}:\n", var_name, var_name));

      if let ValueKind::GlobalAlloc(global_alloc) = var_data.kind() {
        asm.push_str(&generate_global_alloc(self, &global_alloc.init()));
      } else {
        unreachable!("Expected GlobalAlloc for global variable, found {:?}", var_data.kind());
      }
    }

    asm.push_str("\n\n  .text\n");

    for &func in self.func_layout() {
      let func_data = self.func(func);
      if func_data.name().starts_with('@') {
        let func_name = func_data.name().trim_start_matches('@');
        if SYSY_LIB_FUNCTIONS.contains(&func_name) {
          continue; // Skip sysy library functions
        }
      } else {
        unreachable!("Function name should start with '@', found: {}", func_data.name());
      }
      let mut env = Environment::new(self, &global_table);
      env.set_func(func);
      asm.push_str(&func_data.generate_asm(&mut env));
      env.clear_func();
    }
    asm
  }
}

impl GenerateAsm for FunctionData {
  fn generate_asm(&self, env: &mut Environment) -> String {
    env.set_frame_layout(layout_frame(self));

    let mut asm = String::new();
    let func_name = self.name().trim_start_matches('@');
    asm.push_str(&format!("  .globl {}\n{}:\n", func_name, func_name));

    asm.push_str(&env.frame_layout().generate_prologue());

    for (bb, node) in self.layout().bbs() {
      let bb_name = self.dfg().bb(*bb).name();
      match bb_name {
        Some(name) => asm.push_str(&format!("{}:\n", name.trim_start_matches(&['@', '%'][..]))),
        None => unreachable!("Basic block without name"),
      }
      for &inst in node.insts().keys() {
        let value_data = self.dfg().value(inst);
        env.set_inst(inst);
        asm.push_str(&value_data.generate_asm(env));
        env.clear_inst();
      }
    }

    asm.push_str(&env.frame_layout().generate_epilogue(func_name));
    env.clear_frame_layout();

    asm
  }
}



impl GenerateAsm for ValueData {
  fn generate_asm(
    &self,
    env: &mut Environment,
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
      ValueKind::Call(call) => call.generate_asm(env),

      ValueKind::GetElemPtr(get_elem_ptr) => get_elem_ptr.generate_asm(env),
      ValueKind::GetPtr(get_ptr) => get_ptr.generate_asm(env),

      default => {
        // Handle other value kinds if necessary
        format!("  ; Unhandled value kind: {:?}\n", default)
      }
    });
    asm
  }
}

impl GenerateAsm for Integer {
  fn generate_asm(
      &self,
      env: &mut Environment,
    ) -> String {
    let mut asm = String::new();
    asm.push_str(&format!("  li    t0, {}\n", self.value()));
    asm.push_str(&sw("t0", "sp", env.get_self_offset()));
    asm
  }
}

impl GenerateAsm for Load {
  fn generate_asm(
      &self,
      env: &mut Environment,
    ) -> String {
    let mut asm = String::new();
    let ptr = self.src();
    let ptr_reg_guard = RegGuard::new().expect("No register available");
    let mut ptr_reg = ptr_reg_guard.name().to_string();
    asm.push_str(&load_operand_to_reg(env, ptr, &mut ptr_reg));
    if !env.frame_layout().is_ptr(&ptr) {
      asm.push_str(&sw(&ptr_reg, "sp", env.get_self_offset()));
    } else {
      // 此时 ptr_reg 存的是指针的值
      let tmp_reg = RegGuard::new().expect("No register available");
      asm.push_str(&lw(tmp_reg.name(), &ptr_reg, 0));
      asm.push_str(&sw(tmp_reg.name(), "sp", env.get_self_offset()));
    }
    asm
  }
}

impl GenerateAsm for Store {
  fn generate_asm(
      &self,
      env: &mut Environment,
    ) -> String {
    let mut asm = String::new();
    let value = self.value();

    let ptr = self.dest();

    let val_reg_guard = RegGuard::new().expect("No register available");
    let mut val_reg = val_reg_guard.name().to_string();
    asm.push_str(&load_operand_to_reg(env, value, &mut val_reg));
    if ptr.is_global() {
      let addr_reg_guard = RegGuard::new().expect("No register available");
      let mut addr_reg = addr_reg_guard.name().to_string();
      asm.push_str(&env.load_global_addr(ptr, &mut addr_reg));
      asm.push_str(&sw(&val_reg, &addr_reg, 0));
    } else {
      if !env.frame_layout().is_ptr(&ptr) {
        asm.push_str(&sw(&val_reg, "sp", env.get_offset(&ptr)));
      } else {
        // 此时我们需要一个寄存器来存储指针的值
        let ptr_reg_guard = RegGuard::new().expect("No register available");
        let mut ptr_reg = ptr_reg_guard.name().to_string();
        asm.push_str(&load_operand_to_reg(env, ptr, &mut ptr_reg));
        // 现在 ptr_reg 存储的是指针的值
        asm.push_str(&sw(&val_reg, &ptr_reg, 0));
      }
    }
    asm
  }
}

impl GenerateAsm for Return {
  fn generate_asm(
    &self,
    env: &mut Environment,
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
          asm.push_str(&lw("a0", "sp", off));
        }
      }
    }
    asm.push_str(&format!("  j     {}_ret\n", env.cur_func_name()));
    asm
  }
}

impl GenerateAsm for Binary {
  /// 根据 Binary 实例生成 RISC-V 汇编
  fn generate_asm(
    &self,
    env: &mut Environment,
    ) -> String {
    let mut asm = String::new();

    let lhs = self.lhs();
    let rhs = self.rhs();

    let lhs_reg_guard = RegGuard::new().expect("No register available");
    let rhs_reg_guard = RegGuard::new().expect("No register available");

    let mut lhs_reg = lhs_reg_guard.name().to_string();
    let mut rhs_reg = rhs_reg_guard.name().to_string();

    let dest_off = env.get_self_offset();

    asm.push_str(&load_operand_to_reg(env, lhs, &mut lhs_reg));
    asm.push_str(&load_operand_to_reg(env, rhs, &mut rhs_reg));

    use BinaryOp::*;
    
    match self.op() {
      // 相等性比较
      Eq | NotEq => {
        let op_str = if matches!(self.op(), Eq) { "seqz" } else { "snez" };
        asm.push_str(&format!("  sub   {}, {}, {}\n", lhs_reg, lhs_reg, rhs_reg));
        asm.push_str(&format!("  {}   {}, {}\n", op_str, lhs_reg, lhs_reg));
      }
      
      // 小于等于：a <= b 等价于 !(b < a)
      Le => {
        asm.push_str(&format!("  slt   {}, {}, {}\n", lhs_reg, rhs_reg, lhs_reg)); // b < a
        asm.push_str(&format!("  xori  {}, {}, 1\n", lhs_reg, lhs_reg));           // !(b < a)
      }
      
      // 大于等于：a >= b 等价于 !(a < b) 
      Ge => {
        asm.push_str(&format!("  slt   {}, {}, {}\n", lhs_reg, lhs_reg, rhs_reg)); // a < b
        asm.push_str(&format!("  xori  {}, {}, 1\n", lhs_reg, lhs_reg));           // !(a < b)
      }
      
      // 其他运算
      Add => asm.push_str(&format!("  add   {}, {}, {}\n", lhs_reg, lhs_reg, rhs_reg)),
      Sub => asm.push_str(&format!("  sub   {}, {}, {}\n", lhs_reg, lhs_reg, rhs_reg)),
      Mul => asm.push_str(&format!("  mul   {}, {}, {}\n", lhs_reg, lhs_reg, rhs_reg)),
      Div => asm.push_str(&format!("  div   {}, {}, {}\n", lhs_reg, lhs_reg, rhs_reg)),
      Mod => asm.push_str(&format!("  rem   {}, {}, {}\n", lhs_reg, lhs_reg, rhs_reg)),
      And => asm.push_str(&format!("  and   {}, {}, {}\n", lhs_reg, lhs_reg, rhs_reg)),
      Or  => asm.push_str(&format!("  or    {}, {}, {}\n", lhs_reg, lhs_reg, rhs_reg)),
      Xor => asm.push_str(&format!("  xor   {}, {}, {}\n", lhs_reg, lhs_reg, rhs_reg)),
      Shl => asm.push_str(&format!("  sll   {}, {}, {}\n", lhs_reg, lhs_reg, rhs_reg)),
      Shr => asm.push_str(&format!("  srl   {}, {}, {}\n", lhs_reg, lhs_reg, rhs_reg)),
      Sar => asm.push_str(&format!("  sra   {}, {}, {}\n", lhs_reg, lhs_reg, rhs_reg)),
      Lt  => asm.push_str(&format!("  slt   {}, {}, {}\n", lhs_reg, lhs_reg, rhs_reg)),
      Gt  => asm.push_str(&format!("  slt   {}, {}, {}\n", lhs_reg, rhs_reg, lhs_reg)), // b < a
    }

    asm.push_str(&sw(&lhs_reg, "sp", dest_off));

    asm
  }
}

impl GenerateAsm for Branch {
  fn generate_asm(
      &self,
      env: &mut Environment,
    ) -> String {
    let mut asm = String::new();
    let cond = self.cond();

    let cond_reg_guard = RegGuard::new().expect("No register available");
    let mut cond_reg = cond_reg_guard.name().to_string();

    asm.push_str(&load_operand_to_reg(env, cond, &mut cond_reg));
    let then_bb = env.get_bb_name(self.true_bb());
    let else_bb = env.get_bb_name(self.false_bb());
    asm.push_str(&format!("  bnez  {}, {}_tmp_label\n", cond_reg, then_bb));
    asm.push_str(&format!("  j     {}\n", else_bb));
    asm.push_str(&format!("{}_tmp_label:\n", then_bb));
    asm.push_str(&format!("  j     {}\n", then_bb));
    asm
  }
}

impl GenerateAsm for Jump {
  fn generate_asm(
      &self,
      env: &mut Environment,
    ) -> String {
    let mut asm = String::new();
    let target = self.target();
    let target_name = env.get_bb_name(target);
    asm.push_str(&format!("  j     {}\n", target_name));
    asm
  }
}

impl GenerateAsm for Call {
  fn generate_asm(
      &self,
      env: &mut Environment,
    ) -> String {
    let mut asm = String::new();
    let callee = env.get_func_data(self.callee());
    let args = self.args();
    
    for (i, &arg) in args.iter().enumerate() {
      if i < 8 {
        let mut reg = format!("a{}", i);
        asm.push_str(&load_operand_to_reg(env, arg, &mut reg));
      } else {
        let tmp_reg_guard = RegGuard::new().expect("No register available");
        let mut tmp_reg = tmp_reg_guard.name().to_string();
        asm.push_str(&load_operand_to_reg(env, arg, &mut tmp_reg));
        let off = ((i as i32) - 8) * 4;
        asm.push_str(&sw(&tmp_reg, "sp", off));
      }
    }

    let callee_name = callee.name().trim_start_matches("@");
    asm.push_str(&format!("  call  {}\n", callee_name));

    if let TypeKind::Function(_param_ty, ret_ty) = callee.ty().kind() {
      if !ret_ty.is_unit() {
        asm.push_str(&sw("a0", "sp", env.get_self_offset()));
      }
    } else {
      panic!("Call target is not a function type");
    }

    asm
  }
}

impl GenerateAsm for GetElemPtr {
  fn generate_asm(
    &self,
    env: &mut Environment,
    ) -> String {
    let mut asm = String::new();

    let dest_off = env.get_self_offset();

    let src_reg_guard = RegGuard::new().expect("No register available");
    let idx_reg_guard = RegGuard::new().expect("No register available");

    let mut src_reg = src_reg_guard.name().to_string();
    let mut idx_reg = idx_reg_guard.name().to_string();

    if self.src().is_global() {
      asm.push_str(&env.load_global_addr(self.src(), &src_reg));
    } else {
      if !env.frame_layout().is_ptr(&self.src()) {
        let off = env.get_offset(&self.src());
        asm.push_str(&addi(&src_reg, "sp", off));
      } else {
        asm.push_str(&load_operand_to_reg(env, self.src(), &mut src_reg));
      }
    }

    asm.push_str(&load_operand_to_reg(env, self.index(), &mut idx_reg));

    let src_ty = env.get_value_ty(self.src());
    println!("elemptr type: {}", src_ty);

    if let TypeKind::Pointer(base_ty) = src_ty.kind() {
      if let TypeKind::Array(elem_ty, _size) = base_ty.kind() {
        let elem_size = calculate_size(elem_ty.kind().clone(), false);
        let tmp_reg = RegGuard::new().expect("No register available");
        asm.push_str(&format!("  li   {}, {}\n", tmp_reg, elem_size));
        asm.push_str(&format!("  mul   {}, {}, {}\n", idx_reg, idx_reg, tmp_reg));
      } else {
        panic!("Array subscript is not an array type");
      }
    } else {
      panic!("GetElemPtr source is not a pointer type");
    }

    asm.push_str(&format!("  add   {}, {}, {}\n", &src_reg, src_reg, idx_reg));
    asm.push_str(&sw(&src_reg, "sp", dest_off));

    asm
  }
}

impl GenerateAsm for GetPtr {
  fn generate_asm(
    &self,
    env: &mut Environment,
    ) -> String {
    let mut asm = String::new();

    let dest_off = env.get_self_offset();
    println!("GetElemPtr dest offset: {}", dest_off);

    let src_reg_guard = RegGuard::new().expect("No register available");
    let idx_reg_guard = RegGuard::new().expect("No register available");

    let mut src_reg = src_reg_guard.name().to_string();
    let mut idx_reg = idx_reg_guard.name().to_string();

    asm.push_str(&load_operand_to_reg(env, self.src(), &mut src_reg));
    asm.push_str(&load_operand_to_reg(env, self.index(), &mut idx_reg));

    let src_ty = env.get_value_ty(self.src());
    println!("debug type: {}", src_ty);

    if let TypeKind::Pointer(base_ty) = src_ty.kind() {
      if let TypeKind::Array(_, _size) = base_ty.kind() {
        let elem_size = calculate_size(base_ty.kind().clone(), false);
        let tmp_reg = RegGuard::new().expect("No register available");
        asm.push_str(&format!("  li   {}, {}\n", tmp_reg, elem_size));
        asm.push_str(&format!("  mul   {}, {}, {}\n", idx_reg, idx_reg, tmp_reg));
      } else if let TypeKind::Int32 = base_ty.kind() {
        let tmp_reg = RegGuard::new().expect("No register available");
        asm.push_str(&format!("  li   {}, {}\n", tmp_reg, 4));
        asm.push_str(&format!("  mul   {}, {}, {}\n", idx_reg, idx_reg, tmp_reg));
      } else {
        panic!("Array subscript is not an array type");
      }
    } else {
      panic!("GetElemPtr source is not a pointer type");
    }

    asm.push_str(&format!("  add   {}, {}, {}\n", &src_reg, src_reg, idx_reg));
    asm.push_str(&sw(&src_reg, "sp", dest_off));

    asm
  }
}