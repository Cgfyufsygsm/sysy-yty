pub mod asm_gen;
pub mod frame;
pub mod env;
pub mod util;

use crate::backend::asm_gen::GenerateAsm;

pub struct Backend;

impl Backend {
  pub fn generate_asm(program: &koopa::ir::Program) -> String {
    return program.generate_asm();
  }
}