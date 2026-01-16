use std::collections::{HashMap, HashSet};

use koopa::ir::{Function, FunctionData, Program, Value, ValueKind};

pub fn run_dce(program: &mut Program) {
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
