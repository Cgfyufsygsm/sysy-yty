use koopa::ir::Program;

mod cfg;
mod const_prop;
mod dce;

pub fn optimize(program: &mut Program) {
  let mut changed = true;
  while changed {
    changed = false;
    if const_prop::run_const_prop(program) {
      changed = true;
    }
    if cfg::simplify_cfg(program) {
      changed = true;
    }
    if dce::run_dce(program) {
      changed = true;
    }
  }
}
