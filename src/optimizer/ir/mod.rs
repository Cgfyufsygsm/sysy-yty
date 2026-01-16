use koopa::ir::Program;

mod cfg;
mod dce;

pub fn optimize(program: &mut Program) {
  cfg::simplify_cfg(program);
  dce::run_dce(program);
}
