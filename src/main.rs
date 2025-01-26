use clap::Parser;
use lox_bytecode_vm::vm::{InterpretResult, Vm};
use std::{
    error::Error,
    fs,
    io::{self, Write},
    path::PathBuf,
    process,
};

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    /// Optional file path to read a Lox program from
    path: Option<PathBuf>,

    /// Turn debugging information on
    #[arg(short, long)]
    debug: bool,
}

fn main() -> Result<(), Box<dyn Error>> {
    let args = Args::parse();
    let vm = Vm::new(args.debug);

    if let Some(file_path) = args.path {
        run_file(file_path, vm)?;
    } else {
        repl(vm)?;
    }
    Ok(())
}

fn repl(mut vm: Vm) -> Result<(), Box<dyn Error>> {
    let mut buffer = String::new();
    loop {
        print!("> ");
        io::stdout().flush()?;

        match io::stdin().read_line(&mut buffer) {
            Ok(n) => {
                // Empty line will have len 1 because it includes new line char.
                if n == 1 {
                    break;
                }
                vm.interpret(buffer.leak());
                buffer = String::new();
            }
            Err(error) => {
                eprintln!("Error: {error}");
                break;
            }
        }
    }

    Ok(())
}

fn run_file(file_path: PathBuf, mut vm: Vm) -> Result<(), Box<dyn Error>> {
    let source = fs::read_to_string(&file_path)
        .expect(&format!("Failed to read file '{}'", file_path.display()));

    let result = vm.interpret(&source);

    match result {
        InterpretResult::InterpretOk => Ok(()),
        InterpretResult::InterpretCompileError => process::exit(65),
        InterpretResult::InterpretRuntimeError => process::exit(70),
    }
}
