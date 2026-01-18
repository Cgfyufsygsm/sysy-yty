use std::{collections::HashMap, fmt};

use crate::backend::frame::FrameLayout;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Reg {
  Phys(String),
  Virt(u32),
}

impl Reg {
  pub fn phys<S: Into<String>>(name: S) -> Self {
    Reg::Phys(name.into())
  }

  pub fn name(&self) -> &str {
    match self {
      Reg::Phys(name) => name.as_str(),
      Reg::Virt(_) => panic!("virtual register has no physical name"),
    }
  }
}

impl fmt::Display for Reg {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.name())
  }
}

#[derive(Clone, Debug)]
pub enum Inst {
  Li { rd: Reg, imm: i32 },
  La { rd: Reg, symbol: String },
  LwParam { rd: Reg, index: u32 },
  Lw { rd: Reg, base: Reg, offset: i32 },
  Sw { rs: Reg, base: Reg, offset: i32 },
  Add { rd: Reg, rs1: Reg, rs2: Reg },
  Sub { rd: Reg, rs1: Reg, rs2: Reg },
  Mul { rd: Reg, rs1: Reg, rs2: Reg },
  Div { rd: Reg, rs1: Reg, rs2: Reg },
  Rem { rd: Reg, rs1: Reg, rs2: Reg },
  And { rd: Reg, rs1: Reg, rs2: Reg },
  Or { rd: Reg, rs1: Reg, rs2: Reg },
  Xor { rd: Reg, rs1: Reg, rs2: Reg },
  Sll { rd: Reg, rs1: Reg, rs2: Reg },
  Srl { rd: Reg, rs1: Reg, rs2: Reg },
  Sra { rd: Reg, rs1: Reg, rs2: Reg },
  Slt { rd: Reg, rs1: Reg, rs2: Reg },
  Addi { rd: Reg, rs: Reg, imm: i32 },
  Xori { rd: Reg, rs: Reg, imm: i32 },
  Seqz { rd: Reg, rs: Reg },
  Snez { rd: Reg, rs: Reg },
  Bnez { rs: Reg, label: String },
  J { label: String },
  Call { func: String },
  Label { name: String },
  Ret,
  Raw(String),
}

pub struct MachineBlock {
  pub label: String,
  pub insts: Vec<Inst>,
}

pub struct MachineFunction {
  pub name: String,
  pub blocks: Vec<MachineBlock>,
  pub frame: FrameLayout,
  pub vreg_info: VRegInfo,
}

#[derive(Clone, Default)]
pub struct VRegInfo {
  pub spill_offsets: HashMap<u32, i32>,
  pub next_vreg: u32,
}

pub enum GlobalInit {
  Word(i32),
  Zero(usize),
}

pub struct Global {
  pub name: String,
  pub init: Vec<GlobalInit>,
}

pub struct MachineProgram {
  pub globals: Vec<Global>,
  pub functions: Vec<MachineFunction>,
}

impl MachineProgram {
  pub fn emit(&self) -> String {
    let mut asm = String::from("  .data\n");

    for global in &self.globals {
      asm.push_str(&format!("  .globl {}\n{}:\n", global.name, global.name));
      for item in &global.init {
        match item {
          GlobalInit::Word(val) => {
            asm.push_str(&format!("  .word {}\n", val));
          }
          GlobalInit::Zero(size) => {
            asm.push_str(&format!("  .zero {}\n", size));
          }
        }
      }
    }

    asm.push_str("\n\n  .text\n");

    for func in &self.functions {
      asm.push_str(&func.emit());
    }

    asm
  }
}

impl MachineFunction {
  fn emit(&self) -> String {
    let mut asm = String::new();
    asm.push_str(&format!("  .globl {}\n{}:\n", self.name, self.name));
    asm.push_str(&self.frame.generate_prologue());

    for block in &self.blocks {
      asm.push_str(&format!("{}:\n", block.label));
      for inst in &block.insts {
        asm.push_str(&inst.emit());
      }
    }

    asm.push_str(&self.frame.generate_epilogue(&self.name));
    asm
  }
}

impl Inst {
  fn emit(&self) -> String {
    match self {
      Inst::Li { rd, imm } => format!("  li    {}, {}\n", rd, imm),
      Inst::La { rd, symbol } => format!("  la    {}, {}\n", rd, symbol),
      Inst::LwParam { .. } => panic!("LwParam must be resolved before emit"),
      Inst::Lw { rd, base, offset } => emit_lw(rd, base, *offset),
      Inst::Sw { rs, base, offset } => emit_sw(rs, base, *offset),
      Inst::Add { rd, rs1, rs2 } => format!("  add   {}, {}, {}\n", rd, rs1, rs2),
      Inst::Sub { rd, rs1, rs2 } => format!("  sub   {}, {}, {}\n", rd, rs1, rs2),
      Inst::Mul { rd, rs1, rs2 } => format!("  mul   {}, {}, {}\n", rd, rs1, rs2),
      Inst::Div { rd, rs1, rs2 } => format!("  div   {}, {}, {}\n", rd, rs1, rs2),
      Inst::Rem { rd, rs1, rs2 } => format!("  rem   {}, {}, {}\n", rd, rs1, rs2),
      Inst::And { rd, rs1, rs2 } => format!("  and   {}, {}, {}\n", rd, rs1, rs2),
      Inst::Or { rd, rs1, rs2 } => format!("  or    {}, {}, {}\n", rd, rs1, rs2),
      Inst::Xor { rd, rs1, rs2 } => format!("  xor   {}, {}, {}\n", rd, rs1, rs2),
      Inst::Sll { rd, rs1, rs2 } => format!("  sll   {}, {}, {}\n", rd, rs1, rs2),
      Inst::Srl { rd, rs1, rs2 } => format!("  srl   {}, {}, {}\n", rd, rs1, rs2),
      Inst::Sra { rd, rs1, rs2 } => format!("  sra   {}, {}, {}\n", rd, rs1, rs2),
      Inst::Slt { rd, rs1, rs2 } => format!("  slt   {}, {}, {}\n", rd, rs1, rs2),
      Inst::Addi { rd, rs, imm } => emit_addi(rd, rs, *imm),
      Inst::Xori { rd, rs, imm } => format!("  xori  {}, {}, {}\n", rd, rs, imm),
      Inst::Seqz { rd, rs } => format!("  seqz  {}, {}\n", rd, rs),
      Inst::Snez { rd, rs } => format!("  snez  {}, {}\n", rd, rs),
      Inst::Bnez { rs, label } => format!("  bnez  {}, {}\n", rs, label),
      Inst::J { label } => format!("  j     {}\n", label),
      Inst::Call { func } => format!("  call  {}\n", func),
      Inst::Label { name } => format!("{}:\n", name),
      Inst::Ret => "  ret\n".to_string(),
      Inst::Raw(text) => text.clone(),
    }
  }
}

fn imm_fits_12(imm: i32) -> bool {
  imm >= -2048 && imm <= 2047
}

fn pick_scratch(_exclude: &[&Reg]) -> &'static str {
  "t6"
}

fn emit_addi(rd: &Reg, rs: &Reg, imm: i32) -> String {
  if imm_fits_12(imm) {
    format!("  addi  {}, {}, {}\n", rd, rs, imm)
  } else {
    let tmp = pick_scratch(&[rd, rs]);
    format!("  li    {}, {}\n  add   {}, {}, {}\n", tmp, imm, rd, rs, tmp)
  }
}

fn emit_lw(rd: &Reg, base: &Reg, offset: i32) -> String {
  if imm_fits_12(offset) {
    format!("  lw    {}, {}({})\n", rd, offset, base)
  } else {
    let tmp = pick_scratch(&[rd, base]);
    format!(
      "  li    {}, {}\n  add   {}, {}, {}\n  lw    {}, 0({})\n",
      tmp, offset, tmp, tmp, base, rd, tmp
    )
  }
}

fn emit_sw(rs: &Reg, base: &Reg, offset: i32) -> String {
  if imm_fits_12(offset) {
    format!("  sw    {}, {}({})\n", rs, offset, base)
  } else {
    let tmp = pick_scratch(&[rs, base]);
    format!(
      "  li    {}, {}\n  add   {}, {}, {}\n  sw    {}, 0({})\n",
      tmp, offset, tmp, tmp, base, rs, tmp
    )
  }
}
