use std::collections::{HashMap, HashSet};

use koopa::ir::{
  builder::LocalInstBuilder, BasicBlock, Function, FunctionData, Program, Value, ValueKind,
};

pub fn optimize(program: &mut Program) {
  simplify_cfg(program);
  dce_program(program);
}

fn simplify_cfg(program: &mut Program) {
  let funcs: Vec<Function> = program.func_layout().iter().copied().collect();
  for func in funcs {
    let func_data = program.func_mut(func);
    simplify_func_cfg(func_data);
  }
}

fn simplify_func_cfg(data: &mut FunctionData) {
  simplify_branches(data);
  remove_unreachable_blocks(data);
}

fn simplify_branches(data: &mut FunctionData) {
  let mut replacements: Vec<(Value, BasicBlock, Vec<Value>)> = Vec::new();
  let bbs: Vec<BasicBlock> = data.layout().bbs().keys().copied().collect();
  for bb in bbs {
    let node = match data.layout().bbs().node(&bb) {
      Some(node) => node,
      None => continue,
    };
    for &inst in node.insts().keys() {
      let kind = data.dfg().value(inst).kind();
      let ValueKind::Branch(br) = kind else { continue };
      let cond_kind = data.dfg().value(br.cond()).kind();
      if let ValueKind::Integer(intv) = cond_kind {
        let (target, args) = if intv.value() != 0 {
          (br.true_bb(), br.true_args().to_vec())
        } else {
          (br.false_bb(), br.false_args().to_vec())
        };
        replacements.push((inst, target, args));
        continue;
      }
      if br.true_bb() == br.false_bb() && br.true_args() == br.false_args() {
        replacements.push((inst, br.true_bb(), br.true_args().to_vec()));
      }
    }
  }

  for (inst, target, args) in replacements {
    let builder = data.dfg_mut().replace_value_with(inst);
    builder.jump_with_args(target, args);
  }
}

fn remove_unreachable_blocks(data: &mut FunctionData) {
  let Some(entry) = data.layout().entry_bb() else {
    return;
  };
  let reachable = collect_reachable_bbs(data, entry);

  let mut bb_cur = data.layout_mut().bbs_mut().cursor_front_mut();
  while let Some(bb) = bb_cur.key().copied() {
    if reachable.contains(&bb) {
      bb_cur.move_next();
      continue;
    }

    {
      let Some(node) = bb_cur.node_mut() else {
        bb_cur.move_next();
        continue;
      };
      let mut inst_cur = node.insts_mut().cursor_front_mut();
      while inst_cur.key().is_some() {
        inst_cur.remove_current();
      }
    }

    bb_cur.remove_current();
  }
}

fn collect_reachable_bbs(data: &FunctionData, entry: BasicBlock) -> HashSet<BasicBlock> {
  let mut reachable = HashSet::new();
  let mut stack = vec![entry];
  while let Some(bb) = stack.pop() {
    if !reachable.insert(bb) {
      continue;
    }
    let Some(node) = data.layout().bbs().node(&bb) else {
      continue;
    };
    for &inst in node.insts().keys() {
      for succ in data.dfg().value(inst).kind().bb_uses() {
        if !reachable.contains(&succ) {
          stack.push(succ);
        }
      }
    }
  }
  reachable
}

fn dce_program(program: &mut Program) {
  let funcs: Vec<Function> = program.func_layout().iter().copied().collect();
  for func in funcs {
    let func_data = program.func_mut(func);
    let mut dce = DeadCodeElimination::new();
    dce.run_on_func(func, func_data);
  }
}

struct DeadCodeElimination {
  worklist: Vec<Value>,
  liveset: HashSet<Value>,
}

impl DeadCodeElimination {
  fn new() -> Self {
    Self {
      worklist: Vec::new(),
      liveset: HashSet::new(),
    }
  }

  fn run_on_func(&mut self, _func: Function, data: &mut FunctionData) {
    self.worklist.clear();
    self.liveset.clear();
    self.mark(data);
    self.sweep(data);
  }

  fn mark(&mut self, data: &FunctionData) {
    for (v, value) in data.dfg().values() {
      if Self::is_critical_inst(value.kind()) {
        self.liveset.insert(*v);
        self.worklist.push(*v);
      }
    }

    while let Some(inst) = self.worklist.pop() {
      for u in data.dfg().value(inst).kind().value_uses() {
        if u.is_global() {
          continue;
        }
        if !self.liveset.contains(&u) && data.dfg().value(u).kind().is_local_inst() {
          self.liveset.insert(u);
          self.worklist.push(u);
        }
      }
    }
  }

  fn sweep(&self, data: &mut FunctionData) {
    let mut removed = Vec::new();
    let mut bb_cur = data.layout_mut().bbs_mut().cursor_front_mut();
    while let Some(bb) = bb_cur.node_mut() {
      let mut inst_cur = bb.insts_mut().cursor_front_mut();
      while let Some(inst) = inst_cur.key() {
        if !self.liveset.contains(inst) {
          removed.push(*inst);
          inst_cur.remove_current();
        } else {
          inst_cur.move_next();
        }
      }
      bb_cur.move_next();
    }

    if removed.is_empty() {
      return;
    }

    let removed_set: HashSet<Value> = removed.iter().copied().collect();
    let mut user_counts: HashMap<Value, usize> = HashMap::new();
    for &v in &removed {
      let count = data
        .dfg()
        .value(v)
        .used_by()
        .iter()
        .filter(|u| removed_set.contains(u))
        .count();
      user_counts.insert(v, count);
    }

    let mut stack: Vec<Value> = removed
      .iter()
      .filter(|v| user_counts.get(v).copied().unwrap_or(0) == 0)
      .copied()
      .collect();
    let mut removed_dfg = HashSet::new();

    while let Some(v) = stack.pop() {
      let uses: Vec<Value> = data.dfg().value(v).kind().value_uses().collect();
      data.dfg_mut().remove_value(v);
      removed_dfg.insert(v);
      for u in uses {
        if removed_set.contains(&u) {
          if let Some(entry) = user_counts.get_mut(&u) {
            *entry = entry.saturating_sub(1);
            if *entry == 0 {
              stack.push(u);
            }
          }
        }
      }
    }
  }

  fn is_critical_inst(kind: &ValueKind) -> bool {
    matches!(
      kind,
      ValueKind::Store(_)
        | ValueKind::Call(_)
        | ValueKind::Branch(_)
        | ValueKind::Jump(_)
        | ValueKind::Return(_)
    )
  }
}
