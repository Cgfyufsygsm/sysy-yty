use std::ops::Deref;

use koopa::ir::{Program, Value, ValueKind};

use crate::backend::mir::GlobalInit;
use crate::backend::util::calculate_size;

pub(super) fn collect_global_init(program: &Program, value: &Value, out: &mut Vec<GlobalInit>) {
  match program.borrow_value(*value).deref().kind() {
    ValueKind::Integer(int_val) => {
      out.push(GlobalInit::Word(int_val.value()));
    }
    ValueKind::ZeroInit(_) => {
      let ty = program.borrow_value(*value).deref().ty().kind().clone();
      let size = calculate_size(ty, false);
      out.push(GlobalInit::Zero(size));
    }
    ValueKind::Aggregate(aggregate) => {
      for elem in aggregate.elems() {
        collect_global_init(program, elem, out);
      }
    }
    _ => panic!("Expected Integer, ZeroInit or Aggregate for global variable"),
  }
}
