pub mod frontend;
pub mod backend;

use koopa::back::KoopaGenerator;
use lalrpop_util::lalrpop_mod;
use std::fs::{read_to_string, File};
use clap::{Parser, ValueEnum};

lalrpop_mod!(sysy);

/// Output Mode
#[derive(Debug, Clone, ValueEnum)]
enum Mode {
    #[value(name = "-ast")]
    Ast,
    #[value(name = "-koopa")]
    Koopa,
    #[value(name = "-riscv")]
    Riscv,
}

#[derive(Parser, Debug)]
#[command(version, about = "A simple SysY compiler written in Rust")]
struct Cli {
    /// Output mode: -ast for AST, -koopa for Koopa IR, -riscv for RISC-V assembly
    #[arg(value_enum, allow_hyphen_values = true)]
    mode: Mode,

    /// Input SysY source file
    input: String,

    /// Output file for the generated code
    #[arg(short = 'o', long = "output")]
    output: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
  // use clap to parse arguments robustly
  let args = Cli::parse();

  let input = read_to_string(&args.input)?;

  // parse the source code into an AST
  let ast = sysy::CompUnitParser::new().parse(&input).unwrap();

  match args.mode {
    Mode::Ast => {
      std::fs::write(&args.output, format!("{:#?}", ast))?;
    }
    Mode::Koopa => {
      let ir = frontend::Frontend::generate_ir(&ast);
      KoopaGenerator::new(Box::new(File::create(&args.output).unwrap())).generate_on(&ir)?;
    }
    Mode::Riscv => {
      let ir = frontend::Frontend::generate_ir(&ast);
      let asm = backend::Backend::generate_asm(&ir);
      std::fs::write(&args.output, asm)?;
    }
  }

  Ok(())
}