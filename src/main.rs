use std::{fs::File, io::Write, path::PathBuf};

use clap::Parser;
use ctl::Pipeline;

#[derive(Parser)]
struct Arguments {
    input: PathBuf,
    output: Option<PathBuf>,
    #[clap(action, short, long)]
    dump_ast: bool,
}

fn main() -> anyhow::Result<()> {
    let args = Arguments::parse();

    let buffer = std::fs::read_to_string(&args.input)?;
    let result = Pipeline::new(&buffer, args.input)
        .parse()
        .inspect(|ast| {
            if args.dump_ast {
                ast.dump()
            }
        })
        .typecheck()
        .codegen();

    match result {
        Ok(result) => {
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
                eprintln!("{err}");
            }
        }
    }

    Ok(())
}
