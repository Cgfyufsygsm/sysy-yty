use koopa::ir::{
  entities::ValueData,
  values::{
    Binary, BinaryOp, Branch, Call, GetElemPtr, GetPtr, Integer, Jump, Load, Return, Store,
  },
  TypeKind, Value, ValueKind,
};

use crate::backend::{
  env::Environment,
  mir::{Inst, Reg},
  util::calculate_size,
};

pub(super) fn lower_value_data(value: &ValueData, env: &mut Environment) -> Vec<Inst> {
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

fn load_operand_to_reg(env: &mut Environment, val: Value) -> (Reg, Vec<Inst>) {
  if let Some(reg) = env.get_value_reg(val) {
    return (reg, Vec::new());
  }

  if let Some(param_reg) = env.frame_layout().get_param_reg(&val) {
    return (Reg::phys(param_reg.clone()), Vec::new());
  }

  if val.is_global() {
    let global_name = env
      .get_global_name(val)
      .expect("Global variable not found")
      .clone();
    let tmp = env.alloc_temp_reg();
    return (
      tmp.clone(),
      vec![
        Inst::La {
          rd: tmp.clone(),
          symbol: global_name,
        },
        Inst::Lw {
          rd: tmp.clone(),
          base: tmp,
          offset: 0,
        },
      ],
    );
  }

  if let Some(idx) = env
    .func_data()
    .params()
    .iter()
    .position(|&param| param == val)
  {
    if idx >= 8 {
      let tmp = env.alloc_temp_reg();
      return (
        tmp.clone(),
        vec![Inst::LwParam {
          rd: tmp,
          index: (idx - 8) as u32,
        }],
      );
    }
  }

  enum KindTag {
    Integer(i32),
    Alloc,
    Other,
  }

  let kind_tag = match env.func_data().dfg().value(val).kind() {
    ValueKind::Integer(i) => KindTag::Integer(i.value()),
    ValueKind::Alloc(_) => KindTag::Alloc,
    _ => KindTag::Other,
  };

  match kind_tag {
    KindTag::Integer(imm) => {
      let tmp = env.alloc_temp_reg();
      (
        tmp.clone(),
        vec![Inst::Li {
          rd: tmp,
          imm,
        }],
      )
    }
    KindTag::Alloc => {
      let off = env.get_offset(&val);
      let tmp = env.alloc_temp_reg();
      (
        tmp.clone(),
        vec![Inst::Lw {
          rd: tmp,
          base: Reg::phys("sp"),
          offset: off,
        }],
      )
    }
    KindTag::Other => (env.alloc_value_reg(val), Vec::new()),
  }
}

fn lower_integer(int_val: &Integer, env: &mut Environment) -> Vec<Inst> {
  let reg = env.get_self_reg();
  vec![Inst::Li {
    rd: reg,
    imm: int_val.value(),
  }]
}

fn lower_load(load: &Load, env: &mut Environment) -> Vec<Inst> {
  let ptr = load.src();
  let dest_reg = env.get_self_reg();
  let mut insts = Vec::new();

  if ptr.is_global() {
    let addr_reg = env.alloc_temp_reg();
    let global_name = env
      .get_global_name(ptr)
      .expect("Global variable not found")
      .clone();
    insts.push(Inst::La {
      rd: addr_reg.clone(),
      symbol: global_name,
    });
    insts.push(Inst::Lw {
      rd: dest_reg,
      base: addr_reg,
      offset: 0,
    });
    return insts;
  }

  if !env.frame_layout().is_ptr(&ptr) {
    let (val_reg, mut val_insts) = load_operand_to_reg(env, ptr);
    insts.append(&mut val_insts);
    if val_reg != dest_reg {
      insts.push(Inst::Addi {
        rd: dest_reg,
        rs: val_reg,
        imm: 0,
      });
    }
    return insts;
  }

  let (ptr_reg, mut ptr_insts) = load_operand_to_reg(env, ptr);
  insts.append(&mut ptr_insts);
  insts.push(Inst::Lw {
    rd: dest_reg,
    base: ptr_reg,
    offset: 0,
  });

  insts
}

fn lower_store(store: &Store, env: &mut Environment) -> Vec<Inst> {
  let value = store.value();
  let ptr = store.dest();

  let (val_reg, mut insts) = load_operand_to_reg(env, value);

  if ptr.is_global() {
    let addr_reg = env.alloc_temp_reg();
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
    let (ptr_reg, mut ptr_insts) = load_operand_to_reg(env, ptr);
    insts.append(&mut ptr_insts);
    insts.push(Inst::Sw {
      rs: val_reg,
      base: ptr_reg,
      offset: 0,
    });
  }

  insts
}

fn lower_return(ret: &Return, env: &mut Environment) -> Vec<Inst> {
  let mut insts = Vec::new();
  if let Some(v) = ret.value() {
    let vk = env.func_data().dfg().value(v).kind();
    match vk {
      ValueKind::Integer(intv) => insts.push(Inst::Li {
        rd: Reg::phys("a0"),
        imm: intv.value(),
      }),
      _ => {
        let (val_reg, mut load_insts) = load_operand_to_reg(env, v);
        insts.append(&mut load_insts);
        let a0 = Reg::phys("a0");
        if val_reg != a0 {
          insts.push(Inst::Addi {
            rd: a0,
            rs: val_reg,
            imm: 0,
          });
        }
      }
    }
  }
  insts.push(Inst::J {
    label: format!("{}_ret", env.cur_func_name()),
  });
  insts
}

fn lower_binary(bin: &Binary, env: &mut Environment) -> Vec<Inst> {
  let lhs = bin.lhs();
  let rhs = bin.rhs();

  let dest_reg = env.get_self_reg();

  let (lhs_reg, mut insts) = load_operand_to_reg(env, lhs);
  let (rhs_reg, mut rhs_insts) = load_operand_to_reg(env, rhs);
  insts.append(&mut rhs_insts);

  use BinaryOp::*;
  match bin.op() {
    Eq | NotEq => {
      insts.push(Inst::Sub {
        rd: dest_reg.clone(),
        rs1: lhs_reg.clone(),
        rs2: rhs_reg.clone(),
      });
      insts.push(if matches!(bin.op(), Eq) {
        Inst::Seqz {
          rd: dest_reg.clone(),
          rs: dest_reg.clone(),
        }
      } else {
        Inst::Snez {
          rd: dest_reg.clone(),
          rs: dest_reg.clone(),
        }
      });
    }
    Le => {
      insts.push(Inst::Slt {
        rd: dest_reg.clone(),
        rs1: rhs_reg.clone(),
        rs2: lhs_reg.clone(),
      });
      insts.push(Inst::Xori {
        rd: dest_reg.clone(),
        rs: dest_reg.clone(),
        imm: 1,
      });
    }
    Ge => {
      insts.push(Inst::Slt {
        rd: dest_reg.clone(),
        rs1: lhs_reg.clone(),
        rs2: rhs_reg.clone(),
      });
      insts.push(Inst::Xori {
        rd: dest_reg.clone(),
        rs: dest_reg.clone(),
        imm: 1,
      });
    }
    Add => insts.push(Inst::Add {
      rd: dest_reg.clone(),
      rs1: lhs_reg.clone(),
      rs2: rhs_reg.clone(),
    }),
    Sub => insts.push(Inst::Sub {
      rd: dest_reg.clone(),
      rs1: lhs_reg.clone(),
      rs2: rhs_reg.clone(),
    }),
    Mul => insts.push(Inst::Mul {
      rd: dest_reg.clone(),
      rs1: lhs_reg.clone(),
      rs2: rhs_reg.clone(),
    }),
    Div => insts.push(Inst::Div {
      rd: dest_reg.clone(),
      rs1: lhs_reg.clone(),
      rs2: rhs_reg.clone(),
    }),
    Mod => insts.push(Inst::Rem {
      rd: dest_reg.clone(),
      rs1: lhs_reg.clone(),
      rs2: rhs_reg.clone(),
    }),
    And => insts.push(Inst::And {
      rd: dest_reg.clone(),
      rs1: lhs_reg.clone(),
      rs2: rhs_reg.clone(),
    }),
    Or => insts.push(Inst::Or {
      rd: dest_reg.clone(),
      rs1: lhs_reg.clone(),
      rs2: rhs_reg.clone(),
    }),
    Xor => insts.push(Inst::Xor {
      rd: dest_reg.clone(),
      rs1: lhs_reg.clone(),
      rs2: rhs_reg.clone(),
    }),
    Shl => insts.push(Inst::Sll {
      rd: dest_reg.clone(),
      rs1: lhs_reg.clone(),
      rs2: rhs_reg.clone(),
    }),
    Shr => insts.push(Inst::Srl {
      rd: dest_reg.clone(),
      rs1: lhs_reg.clone(),
      rs2: rhs_reg.clone(),
    }),
    Sar => insts.push(Inst::Sra {
      rd: dest_reg.clone(),
      rs1: lhs_reg.clone(),
      rs2: rhs_reg.clone(),
    }),
    Lt => insts.push(Inst::Slt {
      rd: dest_reg.clone(),
      rs1: lhs_reg.clone(),
      rs2: rhs_reg.clone(),
    }),
    Gt => insts.push(Inst::Slt {
      rd: dest_reg.clone(),
      rs1: rhs_reg.clone(),
      rs2: lhs_reg.clone(),
    }),
  }

  insts
}

fn lower_branch(br: &Branch, env: &mut Environment) -> Vec<Inst> {
  let cond = br.cond();
  let (cond_reg, mut insts) = load_operand_to_reg(env, cond);
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

fn lower_call(call: &Call, env: &mut Environment) -> Vec<Inst> {
  let mut insts = Vec::new();
  let callee = env.get_func_data(call.callee());
  let callee_name = callee.name().trim_start_matches('@').to_string();
  let ret_is_unit = if let TypeKind::Function(_param_ty, ret_ty) = callee.ty().kind() {
    ret_ty.is_unit()
  } else {
    panic!("Call target is not a function type");
  };
  let args = call.args();

  for (i, &arg) in args.iter().enumerate() {
    if i < 8 {
      let target = Reg::phys(format!("a{}", i));
      let (arg_reg, mut arg_insts) = load_operand_to_reg(env, arg);
      insts.append(&mut arg_insts);
      if arg_reg != target {
        insts.push(Inst::Addi {
          rd: target,
          rs: arg_reg,
          imm: 0,
        });
      }
    } else {
      let (tmp_reg, mut arg_insts) = load_operand_to_reg(env, arg);
      insts.append(&mut arg_insts);
      let off = (i as i32 - 8) * 4;
      insts.push(Inst::Sw {
        rs: tmp_reg,
        base: Reg::phys("sp"),
        offset: off,
      });
    }
  }

  insts.push(Inst::Call {
    func: callee_name,
  });

  if !ret_is_unit {
    let dest_reg = env.get_self_reg();
    insts.push(Inst::Addi {
      rd: dest_reg,
      rs: Reg::phys("a0"),
      imm: 0,
    });
  }

  insts
}

fn lower_get_elem_ptr(get_elem_ptr: &GetElemPtr, env: &mut Environment) -> Vec<Inst> {
  let dest_reg = env.get_self_reg();
  let mut insts = Vec::new();

  let src_val = get_elem_ptr.src();
  let src_reg = if src_val.is_global() {
    let tmp = env.alloc_temp_reg();
    let global_name = env
      .get_global_name(src_val)
      .expect("Global variable not found")
      .clone();
    insts.push(Inst::La {
      rd: tmp.clone(),
      symbol: global_name,
    });
    tmp
  } else if !env.frame_layout().is_ptr(&src_val) {
    let tmp = env.alloc_temp_reg();
    let off = env.get_offset(&src_val);
    insts.push(Inst::Addi {
      rd: tmp.clone(),
      rs: Reg::phys("sp"),
      imm: off,
    });
    tmp
  } else {
    let (reg, mut src_insts) = load_operand_to_reg(env, src_val);
    insts.append(&mut src_insts);
    reg
  };

  let (idx_reg, mut idx_insts) = load_operand_to_reg(env, get_elem_ptr.index());
  insts.append(&mut idx_insts);

  let src_ty = env.get_value_ty(src_val);
  println!("elemptr type: {}", src_ty);

  let elem_size = if let TypeKind::Pointer(base_ty) = src_ty.kind() {
    if let TypeKind::Array(elem_ty, _size) = base_ty.kind() {
      calculate_size(elem_ty.kind().clone(), false) as i32
    } else {
      panic!("Array subscript is not an array type");
    }
  } else {
    panic!("GetElemPtr source is not a pointer type");
  };

  let size_reg = env.alloc_temp_reg();
  insts.push(Inst::Li {
    rd: size_reg.clone(),
    imm: elem_size,
  });
  let scaled_reg = env.alloc_temp_reg();
  insts.push(Inst::Mul {
    rd: scaled_reg.clone(),
    rs1: idx_reg,
    rs2: size_reg,
  });
  insts.push(Inst::Add {
    rd: dest_reg,
    rs1: src_reg,
    rs2: scaled_reg,
  });

  insts
}

fn lower_get_ptr(get_ptr: &GetPtr, env: &mut Environment) -> Vec<Inst> {
  let dest_reg = env.get_self_reg();
  println!("GetElemPtr dest offset: {}", env.frame_layout().get_offset(env.inst()));

  let mut insts = Vec::new();

  let (src_reg, mut src_insts) = load_operand_to_reg(env, get_ptr.src());
  insts.append(&mut src_insts);

  let (idx_reg, mut idx_insts) = load_operand_to_reg(env, get_ptr.index());
  insts.append(&mut idx_insts);

  let src_ty = env.get_value_ty(get_ptr.src());
  println!("debug type: {}", src_ty);

  let elem_size = if let TypeKind::Pointer(base_ty) = src_ty.kind() {
    if let TypeKind::Array(_, _size) = base_ty.kind() {
      calculate_size(base_ty.kind().clone(), false) as i32
    } else if let TypeKind::Int32 = base_ty.kind() {
      4
    } else {
      panic!("Array subscript is not an array type");
    }
  } else {
    panic!("GetElemPtr source is not a pointer type");
  };

  let size_reg = env.alloc_temp_reg();
  insts.push(Inst::Li {
    rd: size_reg.clone(),
    imm: elem_size,
  });
  let scaled_reg = env.alloc_temp_reg();
  insts.push(Inst::Mul {
    rd: scaled_reg.clone(),
    rs1: idx_reg,
    rs2: size_reg,
  });
  insts.push(Inst::Add {
    rd: dest_reg,
    rs1: src_reg,
    rs2: scaled_reg,
  });

  insts
}
