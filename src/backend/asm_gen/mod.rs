mod function;
mod globals;
mod lower;

use std::collections::HashMap;

use koopa::ir::{Program, ValueKind};

use crate::backend::{
  env::Environment,
  mir::{Global, MachineProgram},
  regalloc,
};

pub trait GenerateProgAsm {
  fn generate_asm(&self) -> String;
}

static SYSY_LIB_FUNCTIONS: [&str; 8] = [
  "getint", "getch", "getarray", "putint", "putch", "putarray", "starttime", "stoptime",
];

impl GenerateProgAsm for Program {
  fn generate_asm(&self) -> String {
    let mut machine = lower_program(self);
    regalloc::allocate_program(&mut machine);
    machine.emit()
  }
}

fn lower_program(program: &Program) -> MachineProgram {
  let mut globals = Vec::new();
  let mut global_table = HashMap::new();

  for &value in program.inst_layout() {
    let var_data = program.borrow_value(value);
    let var_name = var_data
      .name()
      .as_ref()
      .unwrap()
      .trim_start_matches('@');

    global_table.insert(value, var_name.to_string());

    let mut init = Vec::new();
    if let ValueKind::GlobalAlloc(global_alloc) = var_data.kind() {
      globals::collect_global_init(program, &global_alloc.init(), &mut init);
    } else {
      unreachable!("Expected GlobalAlloc for global variable, found {:?}", var_data.kind());
    }

    globals.push(Global {
      name: var_name.to_string(),
      init,
    });
  }

  let mut functions = Vec::new();
  for &func in program.func_layout() {
    let func_data = program.func(func);
    if func_data.name().starts_with('@') {
      let func_name = func_data.name().trim_start_matches('@');
      if SYSY_LIB_FUNCTIONS.contains(&func_name) {
        continue;
      }
    } else {
      unreachable!(
        "Function name should start with '@', found: {}",
        func_data.name()
      );
    }

    let mut env = Environment::new(program, &global_table);
    env.set_func(func);
    functions.push(function::lower_function(func_data, &mut env));
    env.clear_func();
  }

  MachineProgram { globals, functions }
}
