use super::ast::*;
use koopa::ir::{builder::{BasicBlockBuilder, LocalInstBuilder, ValueBuilder}, BasicBlock, FunctionData, Program, Type, Value, Function, BinaryOp::*};

impl CompUnit {
  pub fn generate_on(&self, program: &mut Program) {
    self.func_def.generate_on(program);
  }
}

impl FuncDef {
  pub fn generate_on(&self, program: &mut Program) {
      // Create a new function.

      let func_data = FunctionData::new(
        format!("@{}", self.ident),
        vec![],
        match self.func_type {
          FuncType::Int => Type::get_i32(),
        },
      );
      
      let func = program.new_func(func_data);

        // Create an entry basic block.
      let entry_bb = program.func_mut(func).dfg_mut().new_bb().basic_block(Some("%entry".into()));
      program.func_mut(func).layout_mut().bbs_mut().push_key_back(entry_bb).unwrap();

      // Generate IR for the block.
      self.block.generate_on(program, func, entry_bb);

      // Add the function to the program.
    }
}

impl Block {
  pub fn generate_on(&self, program: &mut Program, func: Function, entry_bb: BasicBlock) {
    self.stmt.generate_on(program, func, entry_bb);
  }
}


impl Stmt {
  pub fn generate_on(&self, program: &mut Program, func: Function, entry_bb: BasicBlock) {
    match self {
      Stmt::Return(exp) => {
        let ret_val = exp.generate_on(program, func, entry_bb);
        // let ret_val = program.func_mut(func).dfg_mut().new_value().integer(*value);
        let ret_inst = program.func_mut(func).dfg_mut().new_value().ret(Some(ret_val));
        program.func_mut(func).layout_mut().bb_mut(entry_bb).insts_mut().push_key_back(ret_inst).unwrap(); 
      }
    }
  }
}

impl Exp {
  pub fn generate_on(&self, program: &mut Program, func: Function, entry_bb: BasicBlock) -> Value {

    match self {
      Exp::Number(n) => {
        program.func_mut(func).dfg_mut().new_value().integer(*n)
      }
      Exp::Unary { op, exp} => {
        let val = exp.generate_on(program, func, entry_bb);

        let inst = match op {
          UnaryOp::Pos => return val,
          UnaryOp::Neg => {
            let zero = program.func_mut(func).dfg_mut().new_value().integer(0);
            let sub_inst = program.func_mut(func).dfg_mut().new_value().binary(Sub, zero, val);
            sub_inst
          }
          UnaryOp::Not => {
            let zero = program.func_mut(func).dfg_mut().new_value().integer(0);
            let cmp_inst = program.func_mut(func).dfg_mut().new_value().binary(Eq, val, zero);
            cmp_inst
          }
        };
        program.func_mut(func).layout_mut().bb_mut(entry_bb).insts_mut().push_key_back(inst).unwrap();
        inst
      }
      Exp::Binary { op, lhs, rhs} => {
        let lhs_val = lhs.generate_on(program, func, entry_bb);
        let rhs_val = rhs.generate_on(program, func, entry_bb);
        let kind = match op {
          BinaryOp::Add => Add,
          BinaryOp::Sub => Sub,
          BinaryOp::Mul => Mul,
          BinaryOp::Div => Div,
          BinaryOp::Mod => Mod,
        };
        let inst = program.func_mut(func).dfg_mut().new_value().binary(kind, lhs_val, rhs_val);
        program.func_mut(func).layout_mut().bb_mut(entry_bb).insts_mut().push_key_back(inst).unwrap();
        inst
      }
    }
  }
}