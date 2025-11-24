pub mod backend;
pub mod frontend;

use clap::{Parser, ValueEnum};
use koopa::back::KoopaGenerator;
use lalrpop_util::lalrpop_mod;
use std::{fs::read_to_string, io::Cursor};

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
    #[value(name = "-perf")]
    Perf,
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
    #[arg(short = 'o', long = "output", required_unless_present = "debug")]
    output: Option<String>,

    /// Print result to stdout instead of writing to a file
    #[arg(long)]
    debug: bool,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // use clap to parse arguments robustly
    let args = Cli::parse();

    let input = read_to_string(&args.input)?;

    // parse the source code into an AST
    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();
    let ir = frontend::Frontend::generate_ir(&ast);

    let output = match args.mode {
        Mode::Ast => {
            format!("{:#?}", ast)
        }
        Mode::Koopa => {
            let mut buf = Cursor::new(Vec::<u8>::new());
            KoopaGenerator::new(Box::new(&mut buf)).generate_on(&ir)?;
            String::from_utf8(buf.into_inner()).unwrap()
        }
        Mode::Riscv => backend::Backend::new().generate_asm(&ir),
        Mode::Perf => backend::Backend::with_options(backend::BackendOptions {
            enable_peephole: true,
            enable_graph_coloring: true,
        })
        .generate_asm(&ir),
    };

    if args.debug {
        print!("{}", output);
    } else {
        std::fs::write(args.output.unwrap(), output)?;
    }

    Ok(())
}
