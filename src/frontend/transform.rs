use koopa::ir::Type;

use super::ast::{BType};

impl Into<Type> for BType {
  fn into(self) -> Type {
    match self {
      BType::Int => Type::get_i32(),
      BType::Void => Type::get_unit(),
    }
  }
}
