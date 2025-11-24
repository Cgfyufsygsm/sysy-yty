pub mod asm_gen;
pub mod env;
pub mod frame;
pub mod pipeline;
pub mod reg;
pub mod util;

use koopa::ir::Type;

use crate::backend::asm_gen::GenerateProgAsm;
pub use pipeline::{AsmPass, AsmPassManager, AsmProgram, BackendOptions};

pub struct Backend {
    options: BackendOptions,
}

impl Backend {
    pub fn new() -> Self {
        Self {
            options: BackendOptions::default(),
        }
    }

    pub fn with_options(options: BackendOptions) -> Self {
        Self { options }
    }

    pub fn generate_asm(&self, program: &koopa::ir::Program) -> String {
        Type::set_ptr_size(4); // Set pointer size to 4 bytes for riscv32
        let mut asm_program = program.generate_asm();
        let mut pass_manager = AsmPassManager::build_from_options(&self.options);
        pass_manager.run_passes(&mut asm_program);
        asm_program.render()
    }
}
