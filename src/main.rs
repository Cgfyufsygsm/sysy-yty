pub mod frontend;
pub mod backend;

use koopa::back::KoopaGenerator;
use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::{read_to_string, File};

// 引用 lalrpop 生成的解析器
// 因为我们刚刚创建了 sysy.lalrpop, 所以模块名是 sysy
lalrpop_mod!(sysy);

fn main() -> Result<(), Box<dyn std::error::Error>> {
  // 解析命令行参数
  let mut args = args();
  args.next();
  let mode = args.next().unwrap();
  let input = args.next().unwrap();
  args.next();
  let output = args.next().unwrap();

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
    _ => {
      eprintln!("Unknown mode: {}", mode);
    }
  }

  Ok(())
}