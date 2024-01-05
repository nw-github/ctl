use clap::Parser;
use ctl::Pipeline;
use std::{fs::File, io::Write, path::{PathBuf, Path}};

#[derive(Parser)]
struct Arguments {
    input: PathBuf,
    output: Option<PathBuf>,
    #[clap(action, short, long)]
    dump_ast: bool,
    #[clap(action, long)]
    no_core: bool,
    #[clap(action, long)]
    no_std: bool,
}

fn main() -> anyhow::Result<()> {
    let args = Arguments::parse();
    let root = Path::new(file!()).parent().unwrap().parent().unwrap();
    let mut libs = Vec::new();
    if !args.no_core {
        libs.push(root.join("ctl/core"));
    }

    if !args.no_std {
        libs.push(root.join("ctl/std"));
    }

    let result = Pipeline::new(args.input.clone(), Default::default())
        .parse()?
        .inspect(|ast| {
            if args.dump_ast {
                ast.dump()
            }
        })
        .typecheck(libs)?
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
        Err(diagnostics) => {
            eprintln!("Compilation failed: ");
            diagnostics.display();
            std::process::exit(1);
        }
    }

    Ok(())
}
