use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct SymbolTable {
    scopes: Vec<HashMap<String, Variable>>,
}

#[derive(Debug, Clone)]
pub enum Variable {
  Const(i32),
  Var,
}

impl SymbolTable {
  pub fn enter_scope(&mut self) {
    self.scopes.push(HashMap::new());
  }

  pub fn leave_scope(&mut self) {
    self.scopes.pop();
  }

  pub fn insert_const(&mut self, ident: &str, value: i32) {
    self.scopes.last_mut().expect("No scope to insert into").insert(ident.to_string(), Variable::Const(value));
  }

  pub fn get(&self, ident: &str) -> Option<&Variable> {
    for scope in self.scopes.iter().rev() {
      if let Some(var) = scope.get(ident) {
        return Some(var);
      }
    }
    None
  }
}