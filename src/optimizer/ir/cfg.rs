use std::collections::HashSet;

use koopa::ir::{
  builder::LocalInstBuilder, BasicBlock, Function, FunctionData, Program, Value, ValueKind,
};

pub fn simplify_cfg(program: &mut Program) -> bool {
  let mut changed = false;
  let funcs: Vec<Function> = program.func_layout().iter().copied().collect();
  for func in funcs {
    let func_data = program.func_mut(func);
    if simplify_func_cfg(func_data) {
      changed = true;
    }
  }
  changed
}

fn simplify_func_cfg(data: &mut FunctionData) -> bool {
  let mut changed = false;
  if simplify_branches(data) {
    changed = true;
  }
  if remove_unreachable_blocks(data) {
    changed = true;
  }
  changed
}

fn simplify_branches(data: &mut FunctionData) -> bool {
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

  let changed = !replacements.is_empty();
  for (inst, target, args) in replacements {
    let builder = data.dfg_mut().replace_value_with(inst);
    builder.jump_with_args(target, args);
  }
  changed
}

fn remove_unreachable_blocks(data: &mut FunctionData) -> bool {
  let Some(entry) = data.layout().entry_bb() else {
    return false;
  };
  let reachable = collect_reachable_bbs(data, entry);
  let mut removed = false;

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
    removed = true;
  }
  removed
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
