use std::collections::{HashMap, HashSet, VecDeque};

use koopa::ir::{
  builder_traits::ValueBuilder, BasicBlock, BinaryOp, Function, FunctionData, Program, Type,
  Value, ValueKind,
};
use koopa::ir::entities::ValueData;

const MAX_ALLOC_FOR_CONST_PROP: usize = 4096;
const MAX_BB_FOR_CONST_PROP: usize = 4096;
const MAX_ALLOC_BB_PRODUCT: u64 = 2_000_000;

pub fn run_const_prop(program: &mut Program) -> bool {
  let mut any_changed = false;
  let funcs: Vec<Function> = program.func_layout().iter().copied().collect();
  for func in funcs {
    let data = program.func_mut(func);
    let mut changed = true;
    let mut func_changed = false;
    while changed {
      changed = false;
      if fold_alloc_loads(data) {
        changed = true;
        func_changed = true;
      }
      if fold_binary(data) {
        changed = true;
        func_changed = true;
      }
      if fold_bb_params(data) {
        changed = true;
        func_changed = true;
      }
    }
    if func_changed {
      any_changed = true;
    }
  }
  any_changed
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
  let bbs: Vec<BasicBlock> = data.layout().bbs().keys().copied().collect();
  let alloc_len = allocs.len();
  let bb_len = bbs.len();
  let product = (alloc_len as u64) * (bb_len as u64);
  if alloc_len > MAX_ALLOC_FOR_CONST_PROP
    || bb_len > MAX_BB_FOR_CONST_PROP
    || product > MAX_ALLOC_BB_PRODUCT
  {
    return false;
  }
  let (preds, succs) = build_cfg_edges(data, &bbs);
  let mut in_map: HashMap<BasicBlock, ConstMap> =
    bbs.iter().map(|bb| (*bb, ConstMap::new())).collect();
  let mut out_map: HashMap<BasicBlock, ConstMap> =
    bbs.iter().map(|bb| (*bb, ConstMap::new())).collect();

  let mut worklist: VecDeque<BasicBlock> = bbs.iter().copied().collect();
  let mut in_queue: HashSet<BasicBlock> = bbs.iter().copied().collect();

  while let Some(bb) = worklist.pop_front() {
    in_queue.remove(&bb);
    let in_state = meet_preds_sparse(&bb, &preds, &out_map);
    in_map.insert(bb, in_state.clone());
    let out_state = transfer_block_sparse(bb, &in_state, data, &allocs);
    let changed = out_map.get(&bb).map_or(true, |old| old != &out_state);
    if changed {
      out_map.insert(bb, out_state);
      if let Some(succs) = succs.get(&bb) {
        for succ in succs {
          if in_queue.insert(*succ) {
            worklist.push_back(*succ);
          }
        }
      }
    }
  }

  let mut replaced_any = false;
  for bb in &bbs {
    let insts: Vec<Value> = match data.layout().bbs().node(bb) {
      Some(node) => node.insts().keys().copied().collect(),
      None => continue,
    };
    let mut state = in_map.get(bb).cloned().unwrap_or_default();
    let mut replacements: Vec<(Value, i32)> = Vec::new();
    let mut to_remove: Vec<Value> = Vec::new();

    for inst in insts {
      match data.dfg().value(inst).kind() {
        ValueKind::Store(store) => {
          let dest = store.dest();
          if !allocs.contains(&dest) {
            continue;
          }
          let val = eval_value_const_sparse(data, store.value(), &state, &allocs);
          match val {
            Some(c) => {
              state.insert(dest, c);
            }
            None => {
              state.remove(&dest);
            }
          }
        }
        ValueKind::Load(load) => {
          let src = load.src();
          let Some(c) = state.get(&src).copied() else { continue };
          if allocs.contains(&src) {
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

type ConstMap = HashMap<Value, i32>;

fn build_cfg_edges(
  data: &FunctionData,
  bbs: &[BasicBlock],
) -> (
  HashMap<BasicBlock, Vec<BasicBlock>>,
  HashMap<BasicBlock, HashSet<BasicBlock>>,
) {
  let mut preds: HashMap<BasicBlock, HashSet<BasicBlock>> =
    bbs.iter().map(|bb| (*bb, HashSet::new())).collect();
  let mut succs: HashMap<BasicBlock, HashSet<BasicBlock>> =
    bbs.iter().map(|bb| (*bb, HashSet::new())).collect();
  for bb in bbs {
    let node = match data.layout().bbs().node(bb) {
      Some(node) => node,
      None => continue,
    };
    for &inst in node.insts().keys() {
      for succ in data.dfg().value(inst).kind().bb_uses() {
        preds.entry(succ).or_default().insert(*bb);
        succs.entry(*bb).or_default().insert(succ);
      }
    }
  }
  let preds = preds
    .into_iter()
    .map(|(bb, set)| (bb, set.into_iter().collect()))
    .collect();
  (preds, succs)
}

fn meet_preds_sparse(
  bb: &BasicBlock,
  preds: &HashMap<BasicBlock, Vec<BasicBlock>>,
  out_map: &HashMap<BasicBlock, ConstMap>,
) -> ConstMap {
  let Some(preds) = preds.get(bb) else {
    return ConstMap::new();
  };
  let mut iter = preds.iter();
  let Some(first) = iter.next() else {
    return ConstMap::new();
  };
  let mut result = out_map.get(first).cloned().unwrap_or_default();
  let empty = ConstMap::new();
  for pred in iter {
    let state = out_map.get(pred).unwrap_or(&empty);
    result.retain(|k, v| state.get(k).map_or(false, |sv| sv == v));
    if result.is_empty() {
      break;
    }
  }
  result
}

fn transfer_block_sparse(
  bb: BasicBlock,
  in_state: &ConstMap,
  data: &FunctionData,
  allocs: &HashSet<Value>,
) -> ConstMap {
  let node = match data.layout().bbs().node(&bb) {
    Some(node) => node,
    None => return in_state.clone(),
  };
  let mut state = in_state.clone();
  for &inst in node.insts().keys() {
    if let ValueKind::Store(store) = data.dfg().value(inst).kind() {
      let dest = store.dest();
      if !allocs.contains(&dest) {
        continue;
      }
      let val = eval_value_const_sparse(data, store.value(), &state, allocs);
      match val {
        Some(c) => {
          state.insert(dest, c);
        }
        None => {
          state.remove(&dest);
        }
      }
    }
  }
  state
}

fn eval_value_const_sparse(
  data: &FunctionData,
  value: Value,
  state: &ConstMap,
  allocs: &HashSet<Value>,
) -> Option<i32> {
  if value.is_global() {
    return None;
  }
  match data.dfg().value(value).kind() {
    ValueKind::Integer(intv) => Some(intv.value()),
    ValueKind::Load(load) => {
      let src = load.src();
      if !allocs.contains(&src) {
        return None;
      }
      state.get(&src).copied()
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
