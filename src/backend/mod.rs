pub mod asm_gen;
pub mod frame;
pub mod env;
pub mod util;
pub mod reg;

use koopa::ir::Type;

use crate::backend::asm_gen::GenerateProgAsm;

pub struct Backend;

impl Backend {
  pub fn generate_asm(program: &koopa::ir::Program) -> String {
    Type::set_ptr_size(4); // Set pointer size to 4 bytes for riscv32
    return program.generate_asm();
  }
}