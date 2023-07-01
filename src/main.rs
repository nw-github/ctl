use anyhow::anyhow;
use clap::Parser;
use ctl::Pipeline;
use std::{
    fs::File,
    io::{Read, Write},
    path::PathBuf,
    process::{Command, Stdio},
};

#[derive(Parser)]
struct Arguments {
    input: PathBuf,
    output: Option<PathBuf>,
    #[clap(action, short, long)]
    dump_ast: bool,
    #[clap(action, short, long)]
    format: bool,
}

fn main() -> anyhow::Result<()> {
    let args = Arguments::parse();

    let buffer = std::fs::read_to_string(&args.input)?;
    let result = Pipeline::new(&buffer, args.input.clone())
        .parse()
        .inspect(|ast| {
            if args.dump_ast {
                ast.dump()
            }
        })
        .typecheck()
        .codegen();

    match result {
        Ok(mut result) => {
            if args.format {
                let command = Command::new("clang-format")
                    .stdin(Stdio::piped())
                    .stdout(Stdio::piped())
                    .spawn()?;
                command
                    .stdin
                    .ok_or(anyhow!("clang-format closed stdin"))?
                    .write_all(result.as_bytes())?;
                result.clear();
                command
                    .stdout
                    .ok_or(anyhow!("clang-format closed stdout"))?
                    .read_to_string(&mut result)?;
            }

            if let Some(output) = args.output {
                let mut output = File::create(output)?;
                output.write_all(result.as_bytes())?;
            } else {
                println!("{result}");
            }
        }
        Err(errors) => {
            eprintln!("Compilation failed: ");
            for err in errors {
                err.display(args.input.to_string_lossy().as_ref());
            }

            std::process::exit(1);
        }
    }

    Ok(())
}
