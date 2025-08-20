use std::collections::HashMap;

use koopa::ir::{Function, Type, TypeKind, Value};

#[derive(Debug, Default)]
pub struct SymbolTable {
  scopes: Vec<HashMap<String, Variable>>,
  global: HashMap<String, GlobalIdent>,
}

#[derive(Debug, Clone)]
pub enum GlobalIdent {
  Variable(Variable),
  Func(Function),
}

#[derive(Debug, Clone)]
pub enum Variable {
  Var(Value),
  Const(i32),
  Array(Value),
  ConstArray(Value),
}

impl SymbolTable {
  pub fn enter_scope(&mut self) {
    self.scopes.push(HashMap::new());
  }

  pub fn leave_scope(&mut self) {
    self.scopes.pop();
  }

  pub fn insert_const(&mut self, ident: &str, value: i32) {
    self.scopes.last_mut().expect("No scope to insert into").insert(ident.into(), Variable::Const(value));
  }

  fn insert_var(&mut self, ident: &str, value: Value) {
    self.scopes.last_mut().expect("No scope to insert into").insert(ident.into(), Variable::Var(value));
  }

  fn insert_arr(&mut self, ident: &str, value: Value) {
    self.scopes.last_mut().expect("No scope to insert into").insert(ident.into(), Variable::Array(value));
  }

  fn insert_const_arr(&mut self, ident: &str, value: Value) {
    self.scopes.last_mut().expect("No scope to insert into").insert(ident.into(), Variable::ConstArray(value));
  }

  pub fn insert_local(&mut self, ident: &str, value: Value, ty: Type, is_const_arr: bool) {
    if is_const_arr {
      self.insert_const_arr(ident, value);
    } else {
      match ty.kind() {
        TypeKind::Array(_, _) => self.insert_arr(ident, value),
        TypeKind::Int32 => self.insert_var(ident, value),
        _ => panic!("Unsupported local variable type: {:?}", ty),
      }
    }
  }

  fn insert_global_var(&mut self, ident: &str, value: Value) {
    self.global.insert(ident.into(), GlobalIdent::Variable(Variable::Var(value)));
  }

  pub fn insert_global_const(&mut self, ident: &str, value: i32) {
    self.global.insert(ident.into(), GlobalIdent::Variable(Variable::Const(value)));
  }

  fn insert_global_arr(&mut self, ident: &str, value: Value) {
    self.global.insert(ident.into(), GlobalIdent::Variable(Variable::Array(value)));
  }

  fn insert_global_const_arr(&mut self, ident: &str, value: Value) {
    self.global.insert(ident.into(), GlobalIdent::Variable(Variable::ConstArray(value)));
  }

  pub fn insert_global(&mut self, ident: &str, value: Value, ty: Type, is_const_arr: bool) {
    if is_const_arr {
      self.insert_global_const_arr(ident, value);
    } else {
      match ty.kind() {
        TypeKind::Array(_, _) => self.insert_global_arr(ident, value),
        TypeKind::Int32 => self.insert_global_var(ident, value),
        _ => panic!("Unsupported global variable type: {:?}", ty),
      }
    }
  }

  pub fn get_var(&self, ident: &str) -> Option<&Variable> {
    for scope in self.scopes.iter().rev() {
      if let Some(var) = scope.get(ident) {
        return Some(var);
      }
    }

    if let Some(global) = self.global.get(ident) {
      return match global {
        GlobalIdent::Variable(var) => Some(var),
        GlobalIdent::Func(_) => unreachable!("Function identifiers should not be queried as variables"),
      }
    }
    None
  }

  pub fn insert_func(&mut self, ident: &str, func: Function) {
    self.global.insert(ident.into(), GlobalIdent::Func(func));
  }

  pub fn get_func(&self, ident: &str) -> Option<&Function> {
    if let Some(GlobalIdent::Func(func)) = self.global.get(ident) {
      Some(func)
    } else {
      None
    }
  }
}