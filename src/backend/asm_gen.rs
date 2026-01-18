use std::{collections::HashMap, ops::Deref};

use koopa::ir::{
  entities::ValueData,
  values::{
    Binary, BinaryOp, Branch, Call, GetElemPtr, GetPtr, Integer, Jump, Load, Return, Store,
  },
  FunctionData, Program, TypeKind, Value, ValueKind,
};

use crate::backend::{
  env::Environment,
  frame::layout_frame,
  mir::{Global, GlobalInit, Inst, MachineBlock, MachineFunction, MachineProgram, Reg},
  reg::RegGuard,
  util::calculate_size,
};

pub trait GenerateProgAsm {
  fn generate_asm(&self) -> String;
}

static SYSY_LIB_FUNCTIONS: [&str; 8] = [
  "getint", "getch", "getarray", "putint", "putch", "putarray", "starttime", "stoptime",
];

fn collect_global_init(program: &Program, value: &Value, out: &mut Vec<GlobalInit>) {
  match program.borrow_value(*value).deref().kind() {
    ValueKind::Integer(int_val) => {
      out.push(GlobalInit::Word(int_val.value()));
    }
    ValueKind::ZeroInit(_) => {
      let ty = program.borrow_value(*value).deref().ty().kind().clone();
      let size = calculate_size(ty, false);
      out.push(GlobalInit::Zero(size));
    }
    ValueKind::Aggregate(aggregate) => {
      for elem in aggregate.elems() {
        collect_global_init(program, elem, out);
      }
    }
    _ => panic!("Expected Integer, ZeroInit or Aggregate for global variable"),
  }
}

impl GenerateProgAsm for Program {
  fn generate_asm(&self) -> String {
    let machine = lower_program(self);
    machine.emit()
  }
}

fn lower_program(program: &Program) -> MachineProgram {
  let mut globals = Vec::new();
  let mut global_table = HashMap::new();

  for &value in program.inst_layout() {
    let var_data = program.borrow_value(value);
    let var_name = var_data
      .name()
      .as_ref()
      .unwrap()
      .trim_start_matches('@');

    global_table.insert(value, var_name.to_string());

    let mut init = Vec::new();
    if let ValueKind::GlobalAlloc(global_alloc) = var_data.kind() {
      collect_global_init(program, &global_alloc.init(), &mut init);
    } else {
      unreachable!("Expected GlobalAlloc for global variable, found {:?}", var_data.kind());
    }

    globals.push(Global {
      name: var_name.to_string(),
      init,
    });
  }

  let mut functions = Vec::new();
  for &func in program.func_layout() {
    let func_data = program.func(func);
    if func_data.name().starts_with('@') {
      let func_name = func_data.name().trim_start_matches('@');
      if SYSY_LIB_FUNCTIONS.contains(&func_name) {
        continue;
      }
    } else {
      unreachable!(
        "Function name should start with '@', found: {}",
        func_data.name()
      );
    }

    let mut env = Environment::new(program, &global_table);
    env.set_func(func);
    functions.push(lower_function(func_data, &mut env));
    env.clear_func();
  }

  MachineProgram { globals, functions }
}

fn lower_function(func: &FunctionData, env: &mut Environment) -> MachineFunction {
  let frame_layout = layout_frame(func);
  env.set_frame_layout(frame_layout.clone());

  let func_name = func.name().trim_start_matches('@').to_string();
  let mut blocks = Vec::new();

  for (bb, node) in func.layout().bbs() {
    let bb_name = func.dfg().bb(*bb).name();
    let label = match bb_name {
      Some(name) => name.trim_start_matches(&['@', '%'][..]).to_string(),
      None => unreachable!("Basic block without name"),
    };
    let mut block = MachineBlock {
      label,
      insts: Vec::new(),
    };

    for &inst in node.insts().keys() {
      let value_data = func.dfg().value(inst);
      env.set_inst(inst);
      block.insts.extend(lower_value_data(value_data, env));
      env.clear_inst();
    }

    blocks.push(block);
  }

  env.clear_frame_layout();

  MachineFunction {
    name: func_name,
    blocks,
    frame: frame_layout,
  }
}

fn lower_value_data(value: &ValueData, env: &mut Environment) -> Vec<Inst> {
  match value.kind() {
    ValueKind::Integer(i) => lower_integer(i, env),
    ValueKind::Load(load) => lower_load(load, env),
    ValueKind::Store(store) => lower_store(store, env),
    ValueKind::Return(ret) => lower_return(ret, env),
    ValueKind::Binary(bin) => lower_binary(bin, env),
    ValueKind::Branch(br) => lower_branch(br, env),
    ValueKind::Jump(jump) => lower_jump(jump, env),
    ValueKind::Alloc(_) => Vec::new(),
    ValueKind::Call(call) => lower_call(call, env),
    ValueKind::GetElemPtr(get_elem_ptr) => lower_get_elem_ptr(get_elem_ptr, env),
    ValueKind::GetPtr(get_ptr) => lower_get_ptr(get_ptr, env),
    default => vec![Inst::Raw(format!("  ; Unhandled value kind: {:?}\n", default))],
  }
}

fn reg_from_guard(guard: &RegGuard) -> Reg {
  Reg::phys(guard.name())
}

fn load_operand_to_reg(env: &Environment, val: Value, reg: Reg) -> (Reg, Vec<Inst>) {
  if let Some(param_reg) = env.frame_layout().get_param_reg(&val) {
    return (Reg::phys(param_reg.clone()), Vec::new());
  }

  if val.is_global() {
    let global_name = env
      .get_global_name(val)
      .expect("Global variable not found")
      .clone();
    return (
      reg.clone(),
      vec![
        Inst::La {
          rd: reg.clone(),
          symbol: global_name,
        },
        Inst::Lw {
          rd: reg.clone(),
          base: reg,
          offset: 0,
        },
      ],
    );
  }

  let data = env.func_data().dfg().value(val);
  match data.kind() {
    ValueKind::Integer(i) => (
      reg.clone(),
      vec![Inst::Li {
        rd: reg,
        imm: i.value(),
      }],
    ),
    _ => {
      let off = env.get_offset(&val);
      (
        reg.clone(),
        vec![Inst::Lw {
          rd: reg,
          base: Reg::phys("sp"),
          offset: off,
        }],
      )
    }
  }
}

fn lower_integer(int_val: &Integer, env: &Environment) -> Vec<Inst> {
  let reg_guard = RegGuard::new().expect("No register available");
  let reg = reg_from_guard(&reg_guard);

  vec![
    Inst::Li {
      rd: reg.clone(),
      imm: int_val.value(),
    },
    Inst::Sw {
      rs: reg,
      base: Reg::phys("sp"),
      offset: env.get_self_offset(),
    },
  ]
}

fn lower_load(load: &Load, env: &Environment) -> Vec<Inst> {
  let ptr = load.src();

  let ptr_guard = RegGuard::new().expect("No register available");
  let ptr_reg = reg_from_guard(&ptr_guard);
  let (ptr_reg, mut insts) = load_operand_to_reg(env, ptr, ptr_reg);

  if !env.frame_layout().is_ptr(&ptr) {
    insts.push(Inst::Sw {
      rs: ptr_reg,
      base: Reg::phys("sp"),
      offset: env.get_self_offset(),
    });
  } else {
    let tmp_guard = RegGuard::new().expect("No register available");
    let tmp_reg = reg_from_guard(&tmp_guard);
    insts.push(Inst::Lw {
      rd: tmp_reg.clone(),
      base: ptr_reg,
      offset: 0,
    });
    insts.push(Inst::Sw {
      rs: tmp_reg,
      base: Reg::phys("sp"),
      offset: env.get_self_offset(),
    });
  }

  insts
}

fn lower_store(store: &Store, env: &Environment) -> Vec<Inst> {
  let value = store.value();
  let ptr = store.dest();

  let val_guard = RegGuard::new().expect("No register available");
  let val_reg = reg_from_guard(&val_guard);
  let (val_reg, mut insts) = load_operand_to_reg(env, value, val_reg);

  if ptr.is_global() {
    let addr_guard = RegGuard::new().expect("No register available");
    let addr_reg = reg_from_guard(&addr_guard);
    let global_name = env
      .get_global_name(ptr)
      .expect("Global variable not found")
      .clone();
    insts.push(Inst::La {
      rd: addr_reg.clone(),
      symbol: global_name,
    });
    insts.push(Inst::Sw {
      rs: val_reg,
      base: addr_reg,
      offset: 0,
    });
  } else if !env.frame_layout().is_ptr(&ptr) {
    insts.push(Inst::Sw {
      rs: val_reg,
      base: Reg::phys("sp"),
      offset: env.get_offset(&ptr),
    });
  } else {
    let ptr_guard = RegGuard::new().expect("No register available");
    let ptr_reg = reg_from_guard(&ptr_guard);
    let (ptr_reg, mut ptr_insts) = load_operand_to_reg(env, ptr, ptr_reg);
    insts.append(&mut ptr_insts);
    insts.push(Inst::Sw {
      rs: val_reg,
      base: ptr_reg,
      offset: 0,
    });
  }

  insts
}

fn lower_return(ret: &Return, env: &Environment) -> Vec<Inst> {
  let mut insts = Vec::new();
  if let Some(v) = ret.value() {
    let vk = env.func_data().dfg().value(v).kind();
    match vk {
      ValueKind::Integer(intv) => insts.push(Inst::Li {
        rd: Reg::phys("a0"),
        imm: intv.value(),
      }),
      _ => {
        let off = env.get_offset(&v);
        insts.push(Inst::Lw {
          rd: Reg::phys("a0"),
          base: Reg::phys("sp"),
          offset: off,
        });
      }
    }
  }
  insts.push(Inst::J {
    label: format!("{}_ret", env.cur_func_name()),
  });
  insts
}

fn lower_binary(bin: &Binary, env: &Environment) -> Vec<Inst> {
  let lhs = bin.lhs();
  let rhs = bin.rhs();

  let lhs_guard = RegGuard::new().expect("No register available");
  let rhs_guard = RegGuard::new().expect("No register available");

  let lhs_reg = reg_from_guard(&lhs_guard);
  let rhs_reg = reg_from_guard(&rhs_guard);

  let dest_off = env.get_self_offset();

  let (lhs_reg, mut insts) = load_operand_to_reg(env, lhs, lhs_reg);
  let (rhs_reg, mut rhs_insts) = load_operand_to_reg(env, rhs, rhs_reg);
  insts.append(&mut rhs_insts);

  use BinaryOp::*;
  match bin.op() {
    Eq | NotEq => {
      let op_inst = if matches!(bin.op(), Eq) {
        Inst::Seqz {
          rd: lhs_reg.clone(),
          rs: lhs_reg.clone(),
        }
      } else {
        Inst::Snez {
          rd: lhs_reg.clone(),
          rs: lhs_reg.clone(),
        }
      };
      insts.push(Inst::Sub {
        rd: lhs_reg.clone(),
        rs1: lhs_reg.clone(),
        rs2: rhs_reg.clone(),
      });
      insts.push(op_inst);
    }
    Le => {
      insts.push(Inst::Slt {
        rd: lhs_reg.clone(),
        rs1: rhs_reg.clone(),
        rs2: lhs_reg.clone(),
      });
      insts.push(Inst::Xori {
        rd: lhs_reg.clone(),
        rs: lhs_reg.clone(),
        imm: 1,
      });
    }
    Ge => {
      insts.push(Inst::Slt {
        rd: lhs_reg.clone(),
        rs1: lhs_reg.clone(),
        rs2: rhs_reg.clone(),
      });
      insts.push(Inst::Xori {
        rd: lhs_reg.clone(),
        rs: lhs_reg.clone(),
        imm: 1,
      });
    }
    Add => insts.push(Inst::Add {
      rd: lhs_reg.clone(),
      rs1: lhs_reg.clone(),
      rs2: rhs_reg.clone(),
    }),
    Sub => insts.push(Inst::Sub {
      rd: lhs_reg.clone(),
      rs1: lhs_reg.clone(),
      rs2: rhs_reg.clone(),
    }),
    Mul => insts.push(Inst::Mul {
      rd: lhs_reg.clone(),
      rs1: lhs_reg.clone(),
      rs2: rhs_reg.clone(),
    }),
    Div => insts.push(Inst::Div {
      rd: lhs_reg.clone(),
      rs1: lhs_reg.clone(),
      rs2: rhs_reg.clone(),
    }),
    Mod => insts.push(Inst::Rem {
      rd: lhs_reg.clone(),
      rs1: lhs_reg.clone(),
      rs2: rhs_reg.clone(),
    }),
    And => insts.push(Inst::And {
      rd: lhs_reg.clone(),
      rs1: lhs_reg.clone(),
      rs2: rhs_reg.clone(),
    }),
    Or => insts.push(Inst::Or {
      rd: lhs_reg.clone(),
      rs1: lhs_reg.clone(),
      rs2: rhs_reg.clone(),
    }),
    Xor => insts.push(Inst::Xor {
      rd: lhs_reg.clone(),
      rs1: lhs_reg.clone(),
      rs2: rhs_reg.clone(),
    }),
    Shl => insts.push(Inst::Sll {
      rd: lhs_reg.clone(),
      rs1: lhs_reg.clone(),
      rs2: rhs_reg.clone(),
    }),
    Shr => insts.push(Inst::Srl {
      rd: lhs_reg.clone(),
      rs1: lhs_reg.clone(),
      rs2: rhs_reg.clone(),
    }),
    Sar => insts.push(Inst::Sra {
      rd: lhs_reg.clone(),
      rs1: lhs_reg.clone(),
      rs2: rhs_reg.clone(),
    }),
    Lt => insts.push(Inst::Slt {
      rd: lhs_reg.clone(),
      rs1: lhs_reg.clone(),
      rs2: rhs_reg.clone(),
    }),
    Gt => insts.push(Inst::Slt {
      rd: lhs_reg.clone(),
      rs1: rhs_reg.clone(),
      rs2: lhs_reg.clone(),
    }),
  }

  insts.push(Inst::Sw {
    rs: lhs_reg,
    base: Reg::phys("sp"),
    offset: dest_off,
  });

  insts
}

fn lower_branch(br: &Branch, env: &Environment) -> Vec<Inst> {
  let cond = br.cond();

  let cond_guard = RegGuard::new().expect("No register available");
  let cond_reg = reg_from_guard(&cond_guard);

  let (cond_reg, mut insts) = load_operand_to_reg(env, cond, cond_reg);
  let then_bb = env.get_bb_name(br.true_bb()).to_string();
  let else_bb = env.get_bb_name(br.false_bb()).to_string();
  let tmp_label = format!("{}_tmp_label", then_bb);

  insts.push(Inst::Bnez {
    rs: cond_reg,
    label: tmp_label.clone(),
  });
  insts.push(Inst::J { label: else_bb });
  insts.push(Inst::Label { name: tmp_label });
  insts.push(Inst::J { label: then_bb });

  insts
}

fn lower_jump(jump: &Jump, env: &Environment) -> Vec<Inst> {
  let target = jump.target();
  let target_name = env.get_bb_name(target);
  vec![Inst::J {
    label: target_name.to_string(),
  }]
}

fn lower_call(call: &Call, env: &Environment) -> Vec<Inst> {
  let mut insts = Vec::new();
  let callee = env.get_func_data(call.callee());
  let args = call.args();

  for (i, &arg) in args.iter().enumerate() {
    if i < 8 {
      let reg = Reg::phys(format!("a{}", i));
      let (_arg_reg, mut arg_insts) = load_operand_to_reg(env, arg, reg);
      insts.append(&mut arg_insts);
    } else {
      let tmp_guard = RegGuard::new().expect("No register available");
      let tmp_reg = reg_from_guard(&tmp_guard);
      let (tmp_reg, mut arg_insts) = load_operand_to_reg(env, arg, tmp_reg);
      insts.append(&mut arg_insts);
      let off = (i as i32 - 8) * 4;
      insts.push(Inst::Sw {
        rs: tmp_reg,
        base: Reg::phys("sp"),
        offset: off,
      });
    }
  }

  let callee_name = callee.name().trim_start_matches('@');
  insts.push(Inst::Call {
    func: callee_name.to_string(),
  });

  if let TypeKind::Function(_param_ty, ret_ty) = callee.ty().kind() {
    if !ret_ty.is_unit() {
      insts.push(Inst::Sw {
        rs: Reg::phys("a0"),
        base: Reg::phys("sp"),
        offset: env.get_self_offset(),
      });
    }
  } else {
    panic!("Call target is not a function type");
  }

  insts
}

fn lower_get_elem_ptr(get_elem_ptr: &GetElemPtr, env: &Environment) -> Vec<Inst> {
  let dest_off = env.get_self_offset();

  let src_guard = RegGuard::new().expect("No register available");
  let idx_guard = RegGuard::new().expect("No register available");

  let mut src_reg = reg_from_guard(&src_guard);
  let mut idx_reg = reg_from_guard(&idx_guard);

  let mut insts = Vec::new();

  if get_elem_ptr.src().is_global() {
    let global_name = env
      .get_global_name(get_elem_ptr.src())
      .expect("Global variable not found")
      .clone();
    insts.push(Inst::La {
      rd: src_reg.clone(),
      symbol: global_name,
    });
  } else if !env.frame_layout().is_ptr(&get_elem_ptr.src()) {
    let off = env.get_offset(&get_elem_ptr.src());
    insts.push(Inst::Addi {
      rd: src_reg.clone(),
      rs: Reg::phys("sp"),
      imm: off,
    });
  } else {
    let (reg, mut src_insts) = load_operand_to_reg(env, get_elem_ptr.src(), src_reg);
    src_reg = reg;
    insts.append(&mut src_insts);
  }

  let (reg, mut idx_insts) = load_operand_to_reg(env, get_elem_ptr.index(), idx_reg);
  idx_reg = reg;
  insts.append(&mut idx_insts);

  let src_ty = env.get_value_ty(get_elem_ptr.src());
  println!("elemptr type: {}", src_ty);

  if let TypeKind::Pointer(base_ty) = src_ty.kind() {
    if let TypeKind::Array(elem_ty, _size) = base_ty.kind() {
      let elem_size = calculate_size(elem_ty.kind().clone(), false) as i32;
      let tmp_guard = RegGuard::new().expect("No register available");
      let tmp_reg = reg_from_guard(&tmp_guard);
      insts.push(Inst::Li {
        rd: tmp_reg.clone(),
        imm: elem_size,
      });
      insts.push(Inst::Mul {
        rd: idx_reg.clone(),
        rs1: idx_reg.clone(),
        rs2: tmp_reg,
      });
    } else {
      panic!("Array subscript is not an array type");
    }
  } else {
    panic!("GetElemPtr source is not a pointer type");
  }

  insts.push(Inst::Add {
    rd: src_reg.clone(),
    rs1: src_reg.clone(),
    rs2: idx_reg,
  });
  insts.push(Inst::Sw {
    rs: src_reg,
    base: Reg::phys("sp"),
    offset: dest_off,
  });

  insts
}

fn lower_get_ptr(get_ptr: &GetPtr, env: &Environment) -> Vec<Inst> {
  let dest_off = env.get_self_offset();
  println!("GetElemPtr dest offset: {}", dest_off);

  let src_guard = RegGuard::new().expect("No register available");
  let idx_guard = RegGuard::new().expect("No register available");

  let mut src_reg = reg_from_guard(&src_guard);
  let mut idx_reg = reg_from_guard(&idx_guard);

  let mut insts = Vec::new();

  let (reg, mut src_insts) = load_operand_to_reg(env, get_ptr.src(), src_reg);
  src_reg = reg;
  insts.append(&mut src_insts);

  let (reg, mut idx_insts) = load_operand_to_reg(env, get_ptr.index(), idx_reg);
  idx_reg = reg;
  insts.append(&mut idx_insts);

  let src_ty = env.get_value_ty(get_ptr.src());
  println!("debug type: {}", src_ty);

  if let TypeKind::Pointer(base_ty) = src_ty.kind() {
    if let TypeKind::Array(_, _size) = base_ty.kind() {
      let elem_size = calculate_size(base_ty.kind().clone(), false) as i32;
      let tmp_guard = RegGuard::new().expect("No register available");
      let tmp_reg = reg_from_guard(&tmp_guard);
      insts.push(Inst::Li {
        rd: tmp_reg.clone(),
        imm: elem_size,
      });
      insts.push(Inst::Mul {
        rd: idx_reg.clone(),
        rs1: idx_reg.clone(),
        rs2: tmp_reg,
      });
    } else if let TypeKind::Int32 = base_ty.kind() {
      let tmp_guard = RegGuard::new().expect("No register available");
      let tmp_reg = reg_from_guard(&tmp_guard);
      insts.push(Inst::Li {
        rd: tmp_reg.clone(),
        imm: 4,
      });
      insts.push(Inst::Mul {
        rd: idx_reg.clone(),
        rs1: idx_reg.clone(),
        rs2: tmp_reg,
      });
    } else {
      panic!("Array subscript is not an array type");
    }
  } else {
    panic!("GetElemPtr source is not a pointer type");
  }

  insts.push(Inst::Add {
    rd: src_reg.clone(),
    rs1: src_reg.clone(),
    rs2: idx_reg,
  });
  insts.push(Inst::Sw {
    rs: src_reg,
    base: Reg::phys("sp"),
    offset: dest_off,
  });

  insts
}
