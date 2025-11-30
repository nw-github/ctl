use anyhow::{Context, Result};
use clap::{Args, Parser, Subcommand, ValueHint};
use ctl::{
    CachingSourceProvider, CodegenFlags, Compiler, Diagnostics, Error, FileId, LspBackend,
    OffsetMode, SourceProvider, UnloadedProject,
};
use std::{
    ffi::OsString,
    fs::File,
    io::Write,
    path::{Path, PathBuf},
    process::{Command, Stdio},
};
use tower_lsp::{lsp_types::Range, LspService, Server};

#[derive(Parser)]
struct Arguments {
    #[command(subcommand)]
    command: SubCommand,

    /// Print a textual representation of the AST to stderr.
    #[clap(action, short, long)]
    #[arg(global = true)]
    dump_ast: bool,

    /// Compile without including the entire standard library.
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
    shared: bool,

    /// Silence unnecessary messages from the compiler
    #[clap(action, short, long)]
    #[arg(global = true)]
    quiet: bool,
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

    #[clap(action, short, long)]
    optimized: bool,

    #[clap(short, long)]
    libs: Vec<String>,
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
    Lsp,
}

fn compile_results(src: &str, leak: bool, output: &Path, build: BuildOrRun) -> Result<()> {
    let warnings = ["-Wall", "-Wextra"];
    let mut cc = Command::new(build.cc)
        .args(["-fwrapv", "-std=c11", "-x", "c", "-", "-o"])
        .arg(output)
        .args(if !leak { &["-lgc"][..] } else { &[] })
        .args(if build.optimized { &["-O2"][..] } else { &[] })
        .args(if build.verbose { &warnings[..] } else { &[] })
        .args(build.ccargs.unwrap_or_default().split(' '))
        .args(build.libs.iter().map(|lib| format!("-l{lib}")))
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

fn display_diagnostics(diag: &Diagnostics) {
    fn format<S: SourceProvider>(
        provider: &mut S,
        diag: &Diagnostics,
        id: FileId,
        errors: &[Error],
        mut format: impl FnMut(&str, Range),
    ) {
        for err in errors.iter().filter(|err| err.span.file == id) {
            // TODO: do something with the errors
            _ = provider.get_source(diag.file_path(err.span.file), |data| {
                format(
                    &err.message,
                    Diagnostics::get_span_range(data, err.span, OffsetMode::Utf32),
                );
            });
        }
    }

    let cwd = std::env::current_dir().ok();
    let mut provider = CachingSourceProvider::new();
    for (id, path) in diag.paths() {
        let path = cwd
            .as_ref()
            .and_then(|cwd| path.strip_prefix(cwd).ok())
            .unwrap_or(path);
        format(&mut provider, diag, id, diag.errors(), |msg, range| {
            eprintln!(
                "error: {}:{}:{}: {msg}",
                path.display(),
                range.start.line + 1,
                range.start.character + 1
            );
        });
    }

    for (id, path) in diag.paths() {
        let path = cwd
            .as_ref()
            .and_then(|cwd| path.strip_prefix(cwd).ok())
            .unwrap_or(path);
        format(&mut provider, diag, id, diag.warnings(), |msg, range| {
            eprintln!(
                "warning: {}:{}:{}: {msg}",
                path.display(),
                range.start.line + 1,
                range.start.character + 1
            );
        });
    }
}

fn main() -> Result<()> {
    let args = Arguments::parse();
    let input = match &args.command {
        SubCommand::Print { input, .. } => input,
        SubCommand::Build { build, .. } => &build.input,
        SubCommand::Run { build, .. } => &build.input,
        SubCommand::Lsp => {
            tokio::runtime::Builder::new_multi_thread()
                .enable_all()
                .build()?
                .block_on(async {
                    let stdin = tokio::io::stdin();
                    let stdout = tokio::io::stdout();
                    let (service, socket) = LspService::new(LspBackend::new);
                    Server::new(stdin, stdout, socket).serve(service).await
                });
            return Ok(());
        }
    };
    let mut proj = UnloadedProject::new(input)?;
    if args.leak {
        // proj.conf.remove_feature("boehm");
    }

    proj.conf.flags = CodegenFlags {
        no_bit_int: args.no_bit_int,
        lib: args.shared,
        minify: matches!(args.command, SubCommand::Print { minify: true, .. }),
    };
    let result = Compiler::new()
        .parse(proj)?
        .inspect(|ast| {
            if args.dump_ast {
                ast.dump()
            }
        })
        .typecheck(Default::default())
        .build();
    let result = match result {
        (Some(code), diag) => {
            if !args.quiet {
                display_diagnostics(&diag);
            }
            code
        }
        (None, diag) => {
            eprintln!("Compilation failed: ");
            display_diagnostics(&diag);
            std::process::exit(1);
        }
    };
    match args.command {
        SubCommand::Print { output, pretty, .. } => {
            if let Some(output) = output {
                let mut output = File::create(output)?;
                print_results(result.as_bytes(), pretty, &mut output)?;
            } else {
                print_results(result.as_bytes(), pretty, &mut std::io::stdout().lock())?;
            }
        }
        SubCommand::Build { build, output } => {
            compile_results(&result, args.leak, &output, build)?;
        }
        SubCommand::Run { build, targs } => {
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
        SubCommand::Lsp => {}
    }

    Ok(())
}
