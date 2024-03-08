use anyhow::{Context, Result};
use clap::{Args, Parser, Subcommand, ValueHint};
use ctl::{Checked, CodegenFlags, Compiler};
use std::{
    ffi::OsString,
    fs::File,
    io::{stdin, Read, Write},
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
    #[clap(action, short = 'g', long)]
    #[arg(global = true)]
    leak: bool,

    /// Compile without using _BitInt/_ExtInt. All integer types will use the type with the nearest
    /// power of two bit count. TODO: proper arithmetic wrapping in this mode
    #[clap(action, short = 'i', long)]
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

        /// Run clang-format on the resulting C code
        #[clap(action, short, long)]
        pretty: bool,

        /// Minify the resulting C code
        #[clap(action, short, long)]
        minify: bool,
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
    #[clap(alias = "l")]
    Lsp {
        path: PathBuf,

        #[clap(action, long)]
        has_file: bool,

        #[clap(action, long)]
        diagnostics: bool,
    },
}

impl SubCommand {
    fn input(&self) -> &Path {
        match self {
            SubCommand::Print { input, .. } => input,
            SubCommand::Lsp { path, .. } => {
                let mut input = path.as_path();
                let mut prev = input;
                while let Some(parent) = prev.parent() {
                    if parent.join("ctl.toml").exists() {
                        input = parent;
                        break;
                    }
                    prev = parent;
                }
                input
            }
            SubCommand::Build { build, .. } => &build.input,
            SubCommand::Run { build, .. } => &build.input,
        }
    }
}

fn compile_results(src: &str, leak: bool, output: &Path, build: BuildOrRun) -> Result<()> {
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

fn print_results(src: &[u8], pretty: bool, output: &mut impl Write) -> Result<()> {
    if pretty {
        let mut cc = Command::new("clang-format")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .context("Couldn't invoke clang-format")?;
        cc.stdin
            .as_mut()
            .context("clang-format closed stdin")?
            .write_all(src)?;
        let result = cc.wait_with_output()?;
        if !result.status.success() {
            anyhow::bail!(
                "clang-format returned non-zero exit code {:?}",
                result.status.code().unwrap_or_default()
            );
        }

        output.write_all(&result.stdout)?;
    } else {
        output.write_all(src)?;
    }

    Ok(())
}

fn transpile(compiler: Compiler<Checked>, flags: CodegenFlags) -> String {
    match compiler.build(flags) {
        Ok(compiler) => {
            compiler.diagnostics().display(compiler.lsp_file());
            compiler.code()
        }
        Err(compiler) => {
            eprintln!("Compilation failed: ");
            compiler.diagnostics().display(compiler.lsp_file());
            std::process::exit(1);
        }
    }
}

fn main() -> Result<()> {
    let mut args = Arguments::parse();
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let core_path = root.join("ctl/core");
    let std_path = root.join("ctl/std");
    let input = args.command.input().to_owned();
    if let Ok(path) = input.canonicalize() {
        if path == core_path {
            args.no_core = true;
            args.no_std = true;
        } else if path == std_path {
            args.no_core = true;
        }
    }

    let mut libs = Vec::new();
    if !args.no_core {
        libs.push(root.join(core_path));
    }

    if !args.no_std {
        libs.push(root.join(std_path));
    }

    let lsp_file = if let SubCommand::Lsp {
        has_file: true,
        path,
        ..
    } = &args.command
    {
        let mut buffer = String::new();
        stdin()
            .read_to_string(&mut buffer)
            .context("reading stdin")?;
        Some((path.clone(), buffer))
    } else {
        None
    };

    let checked = Compiler::new(lsp_file)
        .parse(input)?
        .inspect(|ast| {
            if args.dump_ast {
                ast.dump()
            }
        })
        .typecheck(libs)?;
    let flags = CodegenFlags {
        leak: args.leak,
        no_bit_int: args.no_bit_int,
        lib: args.lib,
        minify: !matches!(args.command, SubCommand::Print { minify: false, .. }),
    };

    match args.command {
        SubCommand::Print { output, pretty, .. } => {
            let result = transpile(checked, flags);
            if let Some(output) = output {
                let mut output = File::create(output)?;
                print_results(result.as_bytes(), pretty, &mut output)?;
            } else {
                print_results(result.as_bytes(), pretty, &mut std::io::stdout().lock())?;
            }
        }
        SubCommand::Build { build, output } => {
            let result = transpile(checked, flags);
            compile_results(&result, args.leak, &output, build)?;
        }
        SubCommand::Run { build, targs } => {
            let result = transpile(checked, flags);
            // TODO: safe?
            let output = Path::new("./a.out");
            compile_results(&result, args.leak, output, build)?;
            #[cfg(unix)]
            {
                use std::os::unix::process::CommandExt;
                return Err(Command::new(output).args(targs).exec().into());
            }

            #[cfg(not(unix))]
            {
                let status = Command::new(output)
                    .args(targs)
                    .spawn()
                    .context("Couldn't invoke the generated program")?
                    .wait()?;
                std::process::exit(status.code().unwrap_or_default());
            }
        }
        SubCommand::Lsp { diagnostics, .. } => {
            if diagnostics {
                checked.diagnostics().display_json(checked.lsp_file());
            }
        }
    }

    Ok(())
}
