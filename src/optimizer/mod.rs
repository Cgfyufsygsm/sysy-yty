pub mod backend;
pub mod ir;

use koopa::ir::Program;

pub fn optimize_ir(program: &mut Program) {
  ir::optimize(program);
}

pub fn optimize_backend(asm: &mut String) {
  backend::optimize(asm);
}
