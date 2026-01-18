use koopa::ir::{builder_traits::ValueBuilder, BinaryOp, Function, FunctionData, Program, Type, ValueKind};

pub fn run_const_prop(program: &mut Program) {
  let funcs: Vec<Function> = program.func_layout().iter().copied().collect();
  for func in funcs {
    let data = program.func_mut(func);
    while fold_binary(data) {}
  }
}

fn fold_binary(data: &mut FunctionData) -> bool {
  let mut evaluated = Vec::new();
  for (v, value) in data.dfg().values() {
    let ValueKind::Binary(bin) = value.kind() else { continue };
    let lhs = data.dfg().value(bin.lhs()).kind();
    let rhs = data.dfg().value(bin.rhs()).kind();
    let (ValueKind::Integer(l), ValueKind::Integer(r)) = (lhs, rhs) else { continue };
    let ans = match bin.op() {
      BinaryOp::NotEq => Some((l.value() != r.value()) as i32),
      BinaryOp::Eq => Some((l.value() == r.value()) as i32),
      BinaryOp::Gt => Some((l.value() > r.value()) as i32),
      BinaryOp::Lt => Some((l.value() < r.value()) as i32),
      BinaryOp::Ge => Some((l.value() >= r.value()) as i32),
      BinaryOp::Le => Some((l.value() <= r.value()) as i32),
      BinaryOp::Add => Some(l.value() + r.value()),
      BinaryOp::Sub => Some(l.value() - r.value()),
      BinaryOp::Mul => Some(l.value() * r.value()),
      BinaryOp::Div => (r.value() != 0).then(|| l.value() / r.value()),
      BinaryOp::Mod => (r.value() != 0).then(|| l.value() % r.value()),
      BinaryOp::And => Some(l.value() & r.value()),
      BinaryOp::Or => Some(l.value() | r.value()),
      BinaryOp::Xor => Some(l.value() ^ r.value()),
      BinaryOp::Shl => Some(l.value() << r.value()),
      BinaryOp::Shr => Some((l.value() as u32 >> r.value()) as i32),
      BinaryOp::Sar => Some(l.value() >> r.value()),
    };
    let Some(bb) = data.layout().parent_bb(*v) else { continue };
    evaluated.push((*v, ans, bb));
  }

  if evaluated.is_empty() {
    return false;
  }

  for (v, ans, _) in &evaluated {
    let builder = data.dfg_mut().replace_value_with(*v);
    if let Some(v) = ans {
      builder.integer(*v);
    } else {
      builder.undef(Type::get_i32());
    }
  }

  for (v, _, bb) in evaluated {
    data.layout_mut().bb_mut(bb).insts_mut().remove(&v);
  }

  true
}
