use koopa::ir::Program;

mod cfg;
mod const_prop;
mod dce;

pub fn optimize(program: &mut Program) {
  const_prop::run_const_prop(program);
  cfg::simplify_cfg(program);
  dce::run_dce(program);
}
