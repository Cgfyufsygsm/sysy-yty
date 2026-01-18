use std::collections::{HashMap, HashSet};

use koopa::ir::{
  builder_traits::ValueBuilder, BasicBlock, BinaryOp, Function, FunctionData, Program, Type,
  Value, ValueKind,
};
use koopa::ir::entities::ValueData;

pub fn run_const_prop(program: &mut Program) {
  let funcs: Vec<Function> = program.func_layout().iter().copied().collect();
  for func in funcs {
    let data = program.func_mut(func);
    let mut changed = true;
    while changed {
      changed = false;
      if fold_alloc_loads(data) {
        changed = true;
      }
      if fold_binary(data) {
        changed = true;
      }
      if fold_bb_params(data) {
        changed = true;
      }
    }
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

fn fold_alloc_loads(data: &mut FunctionData) -> bool {
  let allocs = collect_simple_allocs(data);
  if allocs.is_empty() {
    return false;
  }

  let alloc_list: Vec<Value> = allocs.iter().copied().collect();
  let mut alloc_index: HashMap<Value, usize> = HashMap::new();
  for (i, v) in alloc_list.iter().enumerate() {
    alloc_index.insert(*v, i);
  }

  let bbs: Vec<BasicBlock> = data.layout().bbs().keys().copied().collect();
  let preds = build_preds(data, &bbs);
  let default_state = vec![ConstState::Unknown; alloc_list.len()];
  let mut in_map: HashMap<BasicBlock, Vec<ConstState>> =
    bbs.iter().map(|bb| (*bb, default_state.clone())).collect();
  let mut out_map = in_map.clone();

  let mut changed = true;
  while changed {
    changed = false;
    for bb in &bbs {
      let in_state = meet_preds(bb, &preds, &out_map, &default_state);
      if in_state != *in_map.get(bb).unwrap_or(&default_state) {
        in_map.insert(*bb, in_state.clone());
        changed = true;
      }
      let out_state = transfer_block(*bb, &in_state, data, &alloc_index);
      if out_state != *out_map.get(bb).unwrap_or(&default_state) {
        out_map.insert(*bb, out_state);
        changed = true;
      }
    }
  }

  let mut replaced_any = false;
  for bb in &bbs {
    let insts: Vec<Value> = match data.layout().bbs().node(bb) {
      Some(node) => node.insts().keys().copied().collect(),
      None => continue,
    };
    let mut state = in_map.get(bb).cloned().unwrap_or_else(|| default_state.clone());
    let mut replacements: Vec<(Value, i32)> = Vec::new();
    let mut to_remove: Vec<Value> = Vec::new();

    for inst in insts {
      match data.dfg().value(inst).kind() {
        ValueKind::Store(store) => {
          let dest = store.dest();
          let Some(idx) = alloc_index.get(&dest) else { continue };
          let val = eval_value_const(data, store.value(), &state, &alloc_index);
          state[*idx] = match val {
            Some(c) => ConstState::Const(c),
            None => ConstState::Unknown,
          };
        }
        ValueKind::Load(load) => {
          let src = load.src();
          let Some(idx) = alloc_index.get(&src) else { continue };
          if let ConstState::Const(c) = state[*idx] {
            replacements.push((inst, c));
            to_remove.push(inst);
          }
        }
        _ => {}
      }
    }

    if !replacements.is_empty() {
      for (inst, c) in &replacements {
        let builder = data.dfg_mut().replace_value_with(*inst);
        builder.integer(*c);
      }
      let insts_mut = data.layout_mut().bb_mut(*bb).insts_mut();
      for inst in to_remove {
        insts_mut.remove(&inst);
      }
      replaced_any = true;
    }
  }

  replaced_any
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum ConstState {
  Const(i32),
  Unknown,
}

fn build_preds(
  data: &FunctionData,
  bbs: &[BasicBlock],
) -> HashMap<BasicBlock, Vec<BasicBlock>> {
  let mut preds: HashMap<BasicBlock, HashSet<BasicBlock>> =
    bbs.iter().map(|bb| (*bb, HashSet::new())).collect();
  for bb in bbs {
    let node = match data.layout().bbs().node(bb) {
      Some(node) => node,
      None => continue,
    };
    for &inst in node.insts().keys() {
      for succ in data.dfg().value(inst).kind().bb_uses() {
        preds.entry(succ).or_default().insert(*bb);
      }
    }
  }
  preds
    .into_iter()
    .map(|(bb, set)| (bb, set.into_iter().collect()))
    .collect()
}

fn meet_preds(
  bb: &BasicBlock,
  preds: &HashMap<BasicBlock, Vec<BasicBlock>>,
  out_map: &HashMap<BasicBlock, Vec<ConstState>>,
  default_state: &[ConstState],
) -> Vec<ConstState> {
  let Some(preds) = preds.get(bb) else {
    return default_state.to_vec();
  };
  if preds.is_empty() {
    return default_state.to_vec();
  }

  let mut result = vec![ConstState::Unknown; default_state.len()];
  for idx in 0..default_state.len() {
    let mut val: Option<i32> = None;
    for pred in preds {
      let state = match out_map.get(pred) {
        Some(state) => state.as_slice(),
        None => default_state,
      };
      match state[idx] {
        ConstState::Const(c) => {
          if val.map_or(true, |v| v == c) {
            val = Some(c);
          } else {
            val = None;
            break;
          }
        }
        ConstState::Unknown => {
          val = None;
          break;
        }
      }
    }
    if let Some(c) = val {
      result[idx] = ConstState::Const(c);
    }
  }
  result
}

fn transfer_block(
  bb: BasicBlock,
  in_state: &[ConstState],
  data: &FunctionData,
  alloc_index: &HashMap<Value, usize>,
) -> Vec<ConstState> {
  let node = match data.layout().bbs().node(&bb) {
    Some(node) => node,
    None => return in_state.to_vec(),
  };
  let mut state = in_state.to_vec();
  for &inst in node.insts().keys() {
    if let ValueKind::Store(store) = data.dfg().value(inst).kind() {
      let dest = store.dest();
      let Some(idx) = alloc_index.get(&dest) else { continue };
      let val = eval_value_const(data, store.value(), &state, alloc_index);
      state[*idx] = match val {
        Some(c) => ConstState::Const(c),
        None => ConstState::Unknown,
      };
    }
  }
  state
}

fn eval_value_const(
  data: &FunctionData,
  value: Value,
  state: &[ConstState],
  alloc_index: &HashMap<Value, usize>,
) -> Option<i32> {
  if value.is_global() {
    return None;
  }
  match data.dfg().value(value).kind() {
    ValueKind::Integer(intv) => Some(intv.value()),
    ValueKind::Load(load) => {
      let src = load.src();
      alloc_index.get(&src).and_then(|idx| match state[*idx] {
        ConstState::Const(c) => Some(c),
        ConstState::Unknown => None,
      })
    }
    _ => None,
  }
}

fn collect_simple_allocs(data: &FunctionData) -> HashSet<Value> {
  let mut allocs = HashSet::new();
  for (v, value) in data.dfg().values() {
    if !matches!(value.kind(), ValueKind::Alloc(_)) {
      continue;
    }
    let mut ok = true;
    for user in value.used_by() {
      let kind = data.dfg().value(*user).kind();
      match kind {
        ValueKind::Load(load) => {
          if load.src() != *v {
            ok = false;
            break;
          }
        }
        ValueKind::Store(store) => {
          if store.dest() != *v {
            ok = false;
            break;
          }
        }
        _ => {
          ok = false;
          break;
        }
      }
    }
    if ok {
      allocs.insert(*v);
    }
  }
  allocs
}

fn fold_bb_params(data: &mut FunctionData) -> bool {
  let mut bb_params = Vec::new();
  for (b, bb) in data.dfg().bbs() {
    if bb.used_by().is_empty() {
      continue;
    }
    let mut evaluated = Vec::new();
    'outer: for i in 0..bb.params().len() {
      let param = bb.params()[i];
      if data.dfg().value(param).used_by().is_empty() {
        continue;
      }
      let mut ans: Option<ValueData> = None;
      for user in bb.used_by() {
        let value = match data.dfg().value(*user).kind() {
          ValueKind::Branch(branch) => {
            if branch.true_bb() == *b {
              branch.true_args()[i]
            } else {
              branch.false_args()[i]
            }
          }
          ValueKind::Jump(jump) => jump.args()[i],
          _ => unreachable!("basic block users should be branch or jump"),
        };
        if value.is_global() {
          continue 'outer;
        }
        let value = data.dfg().value(value);
        if !value.kind().is_const() || !ans.as_ref().map_or(true, |v| data.dfg().data_eq(v, value)) {
          continue 'outer;
        }
        ans = Some(value.clone());
      }
      evaluated.push((i, ans.expect("no incoming args for bb param")));
    }
    if !evaluated.is_empty() {
      bb_params.push((*b, evaluated));
    }
  }

  let changed = !bb_params.is_empty();
  for (bb, evals) in bb_params {
    for (i, value) in evals {
      let param = data.dfg().bb(bb).params()[i];
      let param_data = data.dfg().value(param).clone();
      data.dfg_mut().replace_value_with(param).raw(value);
      let new_param = data.dfg_mut().new_value().raw(param_data);
      data.dfg_mut().bb_mut(bb).params_mut()[i] = new_param;
    }
  }

  changed
}
