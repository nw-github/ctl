use clap::Parser;
use ctl::Pipeline;
use std::{fs::File, io::Write, path::PathBuf};

#[derive(Parser)]
struct Arguments {
    input: PathBuf,
    output: Option<PathBuf>,
    #[clap(action, short, long)]
    dump_ast: bool,
}

fn main() -> anyhow::Result<()> {
    let args = Arguments::parse();
    let result = Pipeline::new(args.input.clone())
        .parse()?
        .inspect(|ast| {
            if args.dump_ast {
                ast.dump()
            }
        })
        .typecheck()?
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
            for (file, errors) in errors {
                for err in errors {
                    err.display(&file.to_string_lossy());
                }
            }

            std::process::exit(1);
        }
    }

    Ok(())
}
