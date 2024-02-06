use anyhow::Context;
use clap::{Args, Parser, Subcommand, ValueHint};
use ctl::{CodegenFlags, Compiler};
use std::{
    ffi::OsString,
    fs::File,
    io::Write,
    path::{Path, PathBuf},
    process::{Command, Stdio},
};

#[derive(Parser)]
struct Arguments {
    #[command(subcommand)]
    command: SubCommand,

    /// Print a textual representation of the AST to stderr.
    #[clap(action, short, long)]
    #[arg(global = true)]
    dump_ast: bool,

    /// Debug only. Compile without including the core library.
    #[clap(action, long)]
    #[arg(global = true)]
    no_core: bool,

    /// Compile without including the std library.
    #[clap(action, long)]
    #[arg(global = true)]
    no_std: bool,

    /// Compile without libgc. By default, all memory allocations in this mode will use the libc
    /// allocator and will not be freed until the program exits.
    #[clap(action, long)]
    #[arg(global = true)]
    leak: bool,

    /// Compile without using _BitInt/_ExtInt. All integer types will use the type with the nearest
    /// power of two bit count. TODO: proper arithmetic wrapping in this mode
    #[clap(action, long)]
    #[arg(global = true)]
    no_bit_int: bool,

    /// Compile as a library
    #[clap(action, short, long)]
    #[arg(global = true)]
    lib: bool,
}

#[derive(Args)]
struct BuildOrRun {
    /// The path to the file or project folder
    input: PathBuf,

    /// The C compiler used to generate the binary. Only clang and gcc are officially supported, but
    /// this argument can be used to point to a specific version.
    #[clap(long, default_value = "clang")]
    cc: PathBuf,

    /// Flags to pass to the C compiler, unmodified.
    #[clap(long)]
    ccargs: Option<String>,

    /// View messages from the C compiler
    #[clap(action, short, long)]
    verbose: bool,
}

#[derive(Subcommand)]
enum SubCommand {
    #[clap(alias = "p")]
    Print {
        /// The path to the file or project folder
        input: PathBuf,

        /// The output path for the compilation result. If omitted, the output will be written to
        /// stdout.
        output: Option<PathBuf>,
    },
    #[clap(alias = "b")]
    Build {
        #[clap(flatten)]
        build: BuildOrRun,

        /// The output path for the compiled binary.
        #[clap(default_value = "./a.out")]
        output: PathBuf,
    },
    #[clap(alias = "r")]
    Run {
        #[clap(flatten)]
        build: BuildOrRun,

        /// Command line arguments for the target program
        #[arg(trailing_var_arg = true, value_hint = ValueHint::CommandWithArguments)]
        targs: Vec<OsString>,
    },
}

impl SubCommand {
    fn input(&self) -> &Path {
        match self {
            SubCommand::Print { input, .. } => input,
            SubCommand::Build { build, .. } => &build.input,
            SubCommand::Run { build, .. } => &build.input,
        }
    }
}

fn compile_results(src: &str, leak: bool, output: &Path, build: BuildOrRun) -> anyhow::Result<()> {
    let warnings = ["-Wall", "-Wextra"];
    let mut cc = Command::new(build.cc)
        .arg("-o")
        .arg(output)
        .arg("-std=c11")
        .arg(if leak { "" } else { "-lgc" })
        .args(if build.verbose { &warnings[..] } else { &[] })
        .args(["-x", "c", "-"])
        .args(build.ccargs.unwrap_or_default().split(' '))
        .stdin(Stdio::piped())
        .stdout(if build.verbose {
            Stdio::inherit()
        } else {
            Stdio::null()
        })
        .stderr(if build.verbose {
            Stdio::inherit()
        } else {
            Stdio::null()
        })
        .spawn()
        .context("Couldn't invoke the compiler")?;
    cc.stdin
        .as_mut()
        .context("The C compiler closed stdin")?
        .write_all(src.as_bytes())?;
    let status = cc.wait()?;
    if !status.success() {
        anyhow::bail!(
            "The C compiler returned non-zero exit code {:?}",
            status.code().unwrap_or_default()
        );
    }

    Ok(())
}

fn handle_results(args: Arguments, result: &str) -> anyhow::Result<()> {
    match args.command {
        SubCommand::Print { output, .. } => {
            if let Some(output) = output {
                let mut output = File::create(output)?;
                output.write_all(result.as_bytes())?;
            } else {
                println!("{result}");
            }
        }
        SubCommand::Build { build, output } => {
            compile_results(result, args.leak, &output, build)?;
        }
        SubCommand::Run { build, targs } => {
            // TODO: safe?
            let output = Path::new("./a.out");
            compile_results(result, args.leak, output, build)?;
            Command::new(output)
                .args(targs)
                .spawn()
                .context("Couldn't invoke the generated program")?
                .wait()?;
        }
    }

    Ok(())
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

    let result = Compiler::new(args.command.input().to_owned(), Default::default())
        .parse()?
        .inspect(|ast| {
            if args.dump_ast {
                ast.dump()
            }
        })
        .typecheck(libs)?
        .codegen(CodegenFlags {
            leak: args.leak,
            no_bit_int: args.no_bit_int,
            lib: args.lib,
        });

    match result {
        Ok((diagnostics, result)) => {
            diagnostics.display();
            handle_results(args, &result)
        }
        Err(diagnostics) => {
            eprintln!("Compilation failed: ");
            diagnostics.display();
            std::process::exit(1);
        }
    }
}
