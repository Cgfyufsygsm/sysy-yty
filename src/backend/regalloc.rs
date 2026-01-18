use std::collections::{HashMap, HashSet};

use crate::backend::frame::FrameLayout;
use crate::backend::mir::{Inst, MachineBlock, MachineFunction, MachineProgram, Reg, VRegInfo};

const ALLOC_REGS: [&str; 18] = [
  "t0", "t1", "t2", "t3", "t4", "t5", "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8",
  "s9", "s10", "s11",
];

pub fn allocate_program(program: &mut MachineProgram) {
  for func in &mut program.functions {
    allocate_function(func);
  }
}

fn allocate_function(func: &mut MachineFunction) {
  loop {
    let flat = flatten_blocks(&func.blocks);
    let labels = build_label_map(&flat);
    let (uses, defs) = collect_uses_defs(&flat);
    let succs = build_succs(&flat, &labels);
    let (_in_sets, out_sets) = liveness(&uses, &defs, &succs);
    let force_spill = collect_force_spill(&flat, &out_sets);
    let graph = build_interference(&uses, &defs, &out_sets);
    let (colors, mut spilled) = color_graph(&graph, &force_spill);
    spilled.extend(force_spill);

    if spilled.is_empty() {
      apply_coloring(func, &colors);
      update_callee_saved(func);
      resolve_stack_params(func);
      return;
    }

    rewrite_spills(func, &spilled);
  }
}

fn flatten_blocks(blocks: &[MachineBlock]) -> Vec<Inst> {
  let mut flat = Vec::new();
  for block in blocks {
    flat.push(Inst::Label {
      name: block.label.clone(),
    });
    flat.extend(block.insts.iter().cloned());
  }
  flat
}

fn build_label_map(insts: &[Inst]) -> HashMap<String, usize> {
  let mut labels = HashMap::new();
  for (idx, inst) in insts.iter().enumerate() {
    if let Inst::Label { name } = inst {
      labels.insert(name.clone(), idx);
    }
  }
  labels
}

fn collect_uses_defs(insts: &[Inst]) -> (Vec<HashSet<u32>>, Vec<HashSet<u32>>) {
  let mut uses = Vec::with_capacity(insts.len());
  let mut defs = Vec::with_capacity(insts.len());
  for inst in insts {
    let (u, d) = inst_uses_defs(inst);
    uses.push(u);
    defs.push(d);
  }
  (uses, defs)
}

fn inst_uses_defs(inst: &Inst) -> (HashSet<u32>, HashSet<u32>) {
  let mut uses = HashSet::new();
  let mut defs = HashSet::new();

  let mut use_reg = |reg: &Reg| {
    if let Some(id) = vreg_id(reg) {
      uses.insert(id);
    }
  };
  let mut def_reg = |reg: &Reg| {
    if let Some(id) = vreg_id(reg) {
      defs.insert(id);
    }
  };

  match inst {
    Inst::Li { rd, .. } => def_reg(rd),
    Inst::La { rd, .. } => def_reg(rd),
    Inst::LwParam { rd, .. } => {
      def_reg(rd);
    }
    Inst::Lw { rd, base, .. } => {
      def_reg(rd);
      use_reg(base);
    }
    Inst::Sw { rs, base, .. } => {
      use_reg(rs);
      use_reg(base);
    }
    Inst::Add { rd, rs1, rs2 }
    | Inst::Sub { rd, rs1, rs2 }
    | Inst::Mul { rd, rs1, rs2 }
    | Inst::Div { rd, rs1, rs2 }
    | Inst::Rem { rd, rs1, rs2 }
    | Inst::And { rd, rs1, rs2 }
    | Inst::Or { rd, rs1, rs2 }
    | Inst::Xor { rd, rs1, rs2 }
    | Inst::Sll { rd, rs1, rs2 }
    | Inst::Srl { rd, rs1, rs2 }
    | Inst::Sra { rd, rs1, rs2 }
    | Inst::Slt { rd, rs1, rs2 } => {
      def_reg(rd);
      use_reg(rs1);
      use_reg(rs2);
    }
    Inst::Addi { rd, rs, .. } | Inst::Xori { rd, rs, .. } => {
      def_reg(rd);
      use_reg(rs);
    }
    Inst::Seqz { rd, rs } | Inst::Snez { rd, rs } => {
      def_reg(rd);
      use_reg(rs);
    }
    Inst::Bnez { rs, .. } => use_reg(rs),
    Inst::J { .. }
    | Inst::Call { .. }
    | Inst::Label { .. }
    | Inst::Ret
    | Inst::Raw(_) => {}
  }

  (uses, defs)
}

fn build_succs(insts: &[Inst], labels: &HashMap<String, usize>) -> Vec<Vec<usize>> {
  let mut succs = vec![Vec::new(); insts.len()];
  for (idx, inst) in insts.iter().enumerate() {
    match inst {
      Inst::J { label } => {
        if let Some(&target) = labels.get(label) {
          succs[idx].push(target);
        }
      }
      Inst::Bnez { label, .. } => {
        if let Some(&target) = labels.get(label) {
          succs[idx].push(target);
        }
        if idx + 1 < insts.len() {
          succs[idx].push(idx + 1);
        }
      }
      Inst::Ret => {}
      _ => {
        if idx + 1 < insts.len() {
          succs[idx].push(idx + 1);
        }
      }
    }
  }
  succs
}

fn liveness(
  uses: &[HashSet<u32>],
  defs: &[HashSet<u32>],
  succs: &[Vec<usize>],
) -> (Vec<HashSet<u32>>, Vec<HashSet<u32>>) {
  let mut in_sets = vec![HashSet::new(); uses.len()];
  let mut out_sets = vec![HashSet::new(); uses.len()];

  loop {
    let mut changed = false;
    for i in (0..uses.len()).rev() {
      let mut out = HashSet::new();
      for &succ in &succs[i] {
        out.extend(in_sets[succ].iter().copied());
      }

      let mut new_in = uses[i].clone();
      let mut out_minus_def = out.clone();
      for d in &defs[i] {
        out_minus_def.remove(d);
      }
      new_in.extend(out_minus_def);

      if new_in != in_sets[i] || out != out_sets[i] {
        in_sets[i] = new_in;
        out_sets[i] = out;
        changed = true;
      }
    }

    if !changed {
      break;
    }
  }

  (in_sets, out_sets)
}

fn collect_force_spill(insts: &[Inst], out_sets: &[HashSet<u32>]) -> HashSet<u32> {
  let mut spill = HashSet::new();
  for (idx, inst) in insts.iter().enumerate() {
    if matches!(inst, Inst::Call { .. }) {
      spill.extend(out_sets[idx].iter().copied());
    }
  }
  spill
}

fn build_interference(
  uses: &[HashSet<u32>],
  defs: &[HashSet<u32>],
  outs: &[HashSet<u32>],
) -> HashMap<u32, HashSet<u32>> {
  let mut graph: HashMap<u32, HashSet<u32>> = HashMap::new();
  for i in 0..defs.len() {
    for &v in uses[i].iter().chain(defs[i].iter()) {
      graph.entry(v).or_default();
    }
    for &d in &defs[i] {
      for &l in &outs[i] {
        if d != l {
          graph.entry(d).or_default().insert(l);
          graph.entry(l).or_default().insert(d);
        }
      }
    }
  }
  graph
}

fn color_graph(
  graph: &HashMap<u32, HashSet<u32>>,
  force_spill: &HashSet<u32>,
) -> (HashMap<u32, &'static str>, HashSet<u32>) {
  let mut working = graph.clone();
  for &node in force_spill {
    remove_node(&mut working, node);
  }

  let mut stack = Vec::new();
  let mut spilled = HashSet::new();

  while !working.is_empty() {
    if let Some((&node, _)) = working
      .iter()
      .find(|(_, adj)| adj.len() < ALLOC_REGS.len())
    {
      remove_node(&mut working, node);
      stack.push(node);
    } else {
      let node = choose_spill(&working);
      spilled.insert(node);
      remove_node(&mut working, node);
      stack.push(node);
    }
  }

  let mut colors = HashMap::new();
  while let Some(node) = stack.pop() {
    if spilled.contains(&node) {
      continue;
    }
    let mut used = HashSet::new();
    if let Some(neighbors) = graph.get(&node) {
      for n in neighbors {
        if let Some(&color) = colors.get(n) {
          used.insert(color);
        }
      }
    }

    let mut assigned = None;
    for &reg in &ALLOC_REGS {
      if !used.contains(reg) {
        assigned = Some(reg);
        break;
      }
    }

    if let Some(reg) = assigned {
      colors.insert(node, reg);
    } else {
      spilled.insert(node);
    }
  }

  (colors, spilled)
}

fn remove_node(graph: &mut HashMap<u32, HashSet<u32>>, node: u32) {
  if let Some(neighbors) = graph.remove(&node) {
    for n in neighbors {
      if let Some(adj) = graph.get_mut(&n) {
        adj.remove(&node);
      }
    }
  }
}

fn choose_spill(graph: &HashMap<u32, HashSet<u32>>) -> u32 {
  graph
    .iter()
    .max_by_key(|(_, adj)| adj.len())
    .map(|(&node, _)| node)
    .expect("spill selection on empty graph")
}

fn apply_coloring(func: &mut MachineFunction, colors: &HashMap<u32, &'static str>) {
  for block in &mut func.blocks {
    for inst in &mut block.insts {
      *inst = map_inst_regs(inst, &|reg| match reg {
        Reg::Virt(id) => {
          let phys = colors
            .get(id)
            .unwrap_or_else(|| panic!("missing color for vreg {}", id));
          Reg::phys(*phys)
        }
        _ => reg.clone(),
      });
    }
  }
}

fn update_callee_saved(func: &mut MachineFunction) {
  func.frame.clear_saved_regs();
  let mut used = HashSet::new();
  for block in &func.blocks {
    for inst in &block.insts {
      collect_saved_regs(inst, &mut used);
    }
  }
  let mut regs: Vec<String> = used.into_iter().collect();
  regs.sort();
  for reg in regs {
    func.frame.add_saved_reg(reg);
  }
}

fn resolve_stack_params(func: &mut MachineFunction) {
  let base = func.frame.size();
  for block in &mut func.blocks {
    for inst in &mut block.insts {
      if let Inst::LwParam { rd, index } = inst {
        *inst = Inst::Lw {
          rd: rd.clone(),
          base: Reg::phys("sp"),
          offset: base + (*index as i32) * 4,
        };
      }
    }
  }
}

fn collect_saved_regs(inst: &Inst, out: &mut HashSet<String>) {
  for reg in inst_regs(inst) {
    if let Reg::Phys(name) = reg {
      if is_callee_saved(name) {
        out.insert(name.clone());
      }
    }
  }
}

fn inst_regs(inst: &Inst) -> Vec<&Reg> {
  match inst {
    Inst::Li { rd, .. } => vec![rd],
    Inst::La { rd, .. } => vec![rd],
    Inst::LwParam { rd, .. } => vec![rd],
    Inst::Lw { rd, base, .. } => vec![rd, base],
    Inst::Sw { rs, base, .. } => vec![rs, base],
    Inst::Add { rd, rs1, rs2 }
    | Inst::Sub { rd, rs1, rs2 }
    | Inst::Mul { rd, rs1, rs2 }
    | Inst::Div { rd, rs1, rs2 }
    | Inst::Rem { rd, rs1, rs2 }
    | Inst::And { rd, rs1, rs2 }
    | Inst::Or { rd, rs1, rs2 }
    | Inst::Xor { rd, rs1, rs2 }
    | Inst::Sll { rd, rs1, rs2 }
    | Inst::Srl { rd, rs1, rs2 }
    | Inst::Sra { rd, rs1, rs2 }
    | Inst::Slt { rd, rs1, rs2 } => vec![rd, rs1, rs2],
    Inst::Addi { rd, rs, .. } | Inst::Xori { rd, rs, .. } => vec![rd, rs],
    Inst::Seqz { rd, rs } | Inst::Snez { rd, rs } => vec![rd, rs],
    Inst::Bnez { rs, .. } => vec![rs],
    Inst::J { .. } | Inst::Call { .. } | Inst::Label { .. } | Inst::Ret | Inst::Raw(_) => {
      Vec::new()
    }
  }
}

fn is_callee_saved(name: &str) -> bool {
  matches!(
    name,
    "s0" | "s1"
      | "s2"
      | "s3"
      | "s4"
      | "s5"
      | "s6"
      | "s7"
      | "s8"
      | "s9"
      | "s10"
      | "s11"
  )
}

fn rewrite_spills(func: &mut MachineFunction, spilled: &HashSet<u32>) {
  for block in &mut func.blocks {
    let mut new_insts = Vec::new();
    for inst in &block.insts {
      let (uses, defs) = inst_uses_defs(inst);
      let mut vreg_map: HashMap<u32, u32> = HashMap::new();
      let mut before = Vec::new();
      let mut after = Vec::new();

      for v in uses.iter().filter(|v| spilled.contains(v)) {
        let tmp = alloc_temp_vreg(&mut func.vreg_info);
        vreg_map.insert(*v, tmp);
        let off = ensure_spill_slot(*v, &mut func.vreg_info, &mut func.frame);
        before.push(Inst::Lw {
          rd: Reg::Virt(tmp),
          base: Reg::phys("sp"),
          offset: off,
        });
      }

      for v in defs.iter().filter(|v| spilled.contains(v)) {
        let tmp = *vreg_map.entry(*v).or_insert_with(|| alloc_temp_vreg(&mut func.vreg_info));
        let off = ensure_spill_slot(*v, &mut func.vreg_info, &mut func.frame);
        after.push(Inst::Sw {
          rs: Reg::Virt(tmp),
          base: Reg::phys("sp"),
          offset: off,
        });
      }

      let rewritten = map_inst_regs(inst, &|reg| match reg {
        Reg::Virt(id) => vreg_map
          .get(id)
          .copied()
          .map(Reg::Virt)
          .unwrap_or_else(|| reg.clone()),
        _ => reg.clone(),
      });

      new_insts.extend(before);
      new_insts.push(rewritten);
      new_insts.extend(after);
    }
    block.insts = new_insts;
  }
}

fn ensure_spill_slot(
  vreg: u32,
  vreg_info: &mut VRegInfo,
  frame: &mut FrameLayout,
) -> i32 {
  if let Some(off) = vreg_info.spill_offsets.get(&vreg) {
    return *off;
  }
  let off = frame.alloc_spill_slot();
  vreg_info.spill_offsets.insert(vreg, off);
  off
}

fn alloc_temp_vreg(vreg_info: &mut VRegInfo) -> u32 {
  let id = vreg_info.next_vreg;
  vreg_info.next_vreg += 1;
  id
}

fn map_inst_regs<F>(inst: &Inst, map_reg: &F) -> Inst
where
  F: Fn(&Reg) -> Reg,
{
  match inst {
    Inst::Li { rd, imm } => Inst::Li {
      rd: map_reg(rd),
      imm: *imm,
    },
    Inst::La { rd, symbol } => Inst::La {
      rd: map_reg(rd),
      symbol: symbol.clone(),
    },
    Inst::LwParam { rd, index } => Inst::LwParam {
      rd: map_reg(rd),
      index: *index,
    },
    Inst::Lw { rd, base, offset } => Inst::Lw {
      rd: map_reg(rd),
      base: map_reg(base),
      offset: *offset,
    },
    Inst::Sw { rs, base, offset } => Inst::Sw {
      rs: map_reg(rs),
      base: map_reg(base),
      offset: *offset,
    },
    Inst::Add { rd, rs1, rs2 } => Inst::Add {
      rd: map_reg(rd),
      rs1: map_reg(rs1),
      rs2: map_reg(rs2),
    },
    Inst::Sub { rd, rs1, rs2 } => Inst::Sub {
      rd: map_reg(rd),
      rs1: map_reg(rs1),
      rs2: map_reg(rs2),
    },
    Inst::Mul { rd, rs1, rs2 } => Inst::Mul {
      rd: map_reg(rd),
      rs1: map_reg(rs1),
      rs2: map_reg(rs2),
    },
    Inst::Div { rd, rs1, rs2 } => Inst::Div {
      rd: map_reg(rd),
      rs1: map_reg(rs1),
      rs2: map_reg(rs2),
    },
    Inst::Rem { rd, rs1, rs2 } => Inst::Rem {
      rd: map_reg(rd),
      rs1: map_reg(rs1),
      rs2: map_reg(rs2),
    },
    Inst::And { rd, rs1, rs2 } => Inst::And {
      rd: map_reg(rd),
      rs1: map_reg(rs1),
      rs2: map_reg(rs2),
    },
    Inst::Or { rd, rs1, rs2 } => Inst::Or {
      rd: map_reg(rd),
      rs1: map_reg(rs1),
      rs2: map_reg(rs2),
    },
    Inst::Xor { rd, rs1, rs2 } => Inst::Xor {
      rd: map_reg(rd),
      rs1: map_reg(rs1),
      rs2: map_reg(rs2),
    },
    Inst::Sll { rd, rs1, rs2 } => Inst::Sll {
      rd: map_reg(rd),
      rs1: map_reg(rs1),
      rs2: map_reg(rs2),
    },
    Inst::Srl { rd, rs1, rs2 } => Inst::Srl {
      rd: map_reg(rd),
      rs1: map_reg(rs1),
      rs2: map_reg(rs2),
    },
    Inst::Sra { rd, rs1, rs2 } => Inst::Sra {
      rd: map_reg(rd),
      rs1: map_reg(rs1),
      rs2: map_reg(rs2),
    },
    Inst::Slt { rd, rs1, rs2 } => Inst::Slt {
      rd: map_reg(rd),
      rs1: map_reg(rs1),
      rs2: map_reg(rs2),
    },
    Inst::Addi { rd, rs, imm } => Inst::Addi {
      rd: map_reg(rd),
      rs: map_reg(rs),
      imm: *imm,
    },
    Inst::Xori { rd, rs, imm } => Inst::Xori {
      rd: map_reg(rd),
      rs: map_reg(rs),
      imm: *imm,
    },
    Inst::Seqz { rd, rs } => Inst::Seqz {
      rd: map_reg(rd),
      rs: map_reg(rs),
    },
    Inst::Snez { rd, rs } => Inst::Snez {
      rd: map_reg(rd),
      rs: map_reg(rs),
    },
    Inst::Bnez { rs, label } => Inst::Bnez {
      rs: map_reg(rs),
      label: label.clone(),
    },
    Inst::J { label } => Inst::J {
      label: label.clone(),
    },
    Inst::Call { func } => Inst::Call {
      func: func.clone(),
    },
    Inst::Label { name } => Inst::Label {
      name: name.clone(),
    },
    Inst::Ret => Inst::Ret,
    Inst::Raw(text) => Inst::Raw(text.clone()),
  }
}

fn vreg_id(reg: &Reg) -> Option<u32> {
  match reg {
    Reg::Virt(id) => Some(*id),
    _ => None,
  }
}
