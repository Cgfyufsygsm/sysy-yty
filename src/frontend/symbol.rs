use std::collections::HashMap;

use koopa::ir::{Function, Value};

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
  Const(i32),
  Var(Value),
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

  pub fn insert_var(&mut self, ident: &str, value: Value) {
    self.scopes.last_mut().expect("No scope to insert into").insert(ident.into(), Variable::Var(value));
  }

  pub fn insert_global_var(&mut self, ident: &str, value: Value) {
    self.global.insert(ident.into(), GlobalIdent::Variable(Variable::Var(value)));
  }

  pub fn insert_global_const(&mut self, ident: &str, value: i32) {
    self.global.insert(ident.into(), GlobalIdent::Variable(Variable::Const(value)));
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