pub mod ir_gen;
pub mod ast;
pub mod symbol;
pub mod env;

use koopa::ir::Program;
use crate::frontend::ast::CompUnit; // Add this line to import CompUnit
use crate::frontend::ir_gen::GenerateIR;

pub struct Frontend;

impl Frontend {
  pub fn generate_ir(ast: &CompUnit) -> Program {
    let mut env = env::Environment::default();
    ast.generate_on(&mut env);
    env.ctx.program
  }
}