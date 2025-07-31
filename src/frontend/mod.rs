pub mod ir_gen;
pub mod ast;

use koopa::ir::Program;
use crate::frontend::ast::CompUnit; // Add this line to import CompUnit

pub struct Frontend;

impl Frontend {
  pub fn generate_ir(ast: &CompUnit) -> Program {
    let mut program = Program::new();
    ast.generate_on(&mut program);
    program
  }
}