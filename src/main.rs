pub mod frontend;
pub mod backend;

use koopa::back::KoopaGenerator;
use lalrpop_util::lalrpop_mod;
use std::fs::{read_to_string, File};
use clap::{Arg, Command};

lalrpop_mod!(sysy);

fn main() -> Result<(), Box<dyn std::error::Error>> {
  let matches = get_arg_matches();

  let mode = matches.get_one::<String>("mode").unwrap();
  let input = matches.get_one::<String>("input").unwrap();
  let output = matches.get_one::<String>("output").unwrap();

  // 读取输入文件
  let input = read_to_string(input)?;

  // 调用 lalrpop 生成的 parser 解析输入文件
  let ast = sysy::CompUnitParser::new().parse(&input).unwrap();
  println!("{:#?}", ast);

  match mode.as_str() {
    "-ast" => {
      println!("{:#?}", ast);
    }
    "-koopa" => {
      let ir = frontend::Frontend::generate_ir(&ast);
      KoopaGenerator::new(Box::new(File::create(output).unwrap())).generate_on(&ir)?;
    }
    "-riscv" => {
      let ir = frontend::Frontend::generate_ir(&ast);
      let asm = backend::Backend::generate_asm(&ir);
      std::fs::write(output, asm)?;
    }
    _ => unreachable!("Mode validation is handled by clap"),
  }

  Ok(())
}

fn get_arg_matches() -> clap::ArgMatches {
  Command::new("SysY Compiler")
    .allow_hyphen_values(true) // allow hyphen values like -ast, -koopa, -riscv
    .version("0.1.0")
    .about("A simple SysY compiler written in Rust")
    .arg(
      Arg::new("mode")
        .required(true)
        .value_parser(["-ast", "-koopa", "-riscv"])
        .help("Output mode: -ast for AST, -koopa for Koopa IR, -riscv for RISC-V assembly"),
    )
    .arg(
      Arg::new("input")
        .required(true)
        .help("Input SysY source file"),
    )
    .arg(
      Arg::new("output")
        .required(true)
        .help("Output path for the generated code"),
    )
    .get_matches()
}