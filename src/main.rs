use anyhow::{Context, Result};
use clap::{Args, Parser, Subcommand, ValueHint};
use colored::Colorize;
use ctl::{
    CachingSourceProvider, Compiler, Configuration, Diagnostics, Error, FileId, LspBackend,
    OffsetMode, SourceProvider, UnloadedProject, intern::Strings,
};
use std::{
    ffi::OsString,
    fs::File,
    io::Write,
    path::{Path, PathBuf},
    process::{Command, Stdio},
};
use tower_lsp::{LspService, Server, lsp_types::Range};

#[derive(Parser)]
struct Arguments {
    #[command(subcommand)]
    command: SubCommand,

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
    shared: Option<bool>,

    /// Silence unnecessary messages from the compiler
    #[clap(action, short, long)]
    #[arg(global = true)]
    quiet: bool,

    /// View messages from the C compiler
    #[clap(action, short, long)]
    verbose: bool,

    /// Minify the resulting C code
    #[clap(action, short, long)]
    minify: bool,
}

#[derive(Args)]
struct BuildOrRun {
    /// The path to the file or project folder
    input: Option<PathBuf>,

    /// The C compiler used to generate the binary. Only clang and gcc are officially supported, but
    /// this argument can be used to point to a specific version.
    #[clap(long, default_value = "clang")]
    cc: PathBuf,

    /// Flags to pass to the C compiler, unmodified.
    #[clap(long)]
    ccargs: Option<String>,

    #[clap(
        short,
        long,
        num_args = 0..=1,
        default_missing_value = "3",
        value_parser = clap::value_parser!(i32).range(0..=3),
    )]
    opt_level: Option<i32>,

    /// Include debug info and assertions
    #[clap(action, short, long)]
    debug: bool,

    /// The output path for the compiled artifacts.
    #[clap(long)]
    out_dir: Option<PathBuf>,

    /// Pass the C source code to the compiler via stdin.
    #[clap(action, long)]
    use_stdin: bool,

    /// Run clang-format on the resulting C code
    #[clap(action, short, long)]
    pretty: bool,

    #[clap(long)]
    libs: Vec<String>,
}

#[derive(Subcommand)]
enum SubCommand {
    #[clap(alias = "p")]
    Print {
        /// The path to the file or project folder
        input: Option<PathBuf>,

        /// The output path for the compilation result. If omitted, the output will be written to
        /// stdout.
        #[clap(short, long)]
        output: Option<PathBuf>,

        /// Do not run clang-format on the resulting C code
        #[clap(action, short, long)]
        ugly: bool,

        /// Print the code in test mode
        #[clap(action, short, long)]
        test: bool,
    },
    #[clap(alias = "b")]
    Build {
        #[clap(flatten)]
        build: BuildOrRun,
    },
    #[clap(alias = "r")]
    Run {
        #[clap(flatten)]
        build: BuildOrRun,

        /// Command line arguments for the target program
        #[arg(trailing_var_arg = true, value_hint = ValueHint::CommandWithArguments)]
        targs: Vec<OsString>,
    },
    #[clap(alias = "d")]
    Dump {
        /// The path to the file or project folder
        input: Option<PathBuf>,

        #[clap(action, short, long)]
        tokens: bool,

        /// The modules to dump. If empty, the module corresponding to the input project will be dumped.
        modules: Vec<String>,
    },
    #[clap(alias = "t")]
    Test {
        #[clap(flatten)]
        build: BuildOrRun,
    },
    Lsp,
}

trait LogIfVerbose {
    fn log_if_verbose(&mut self, verbose: bool) -> &mut Self;
}

impl LogIfVerbose for Command {
    fn log_if_verbose(&mut self, verbose: bool) -> &mut Self {
        if verbose {
            eprint!("{} \"{}\"", "Executing command:".cyan(), self.get_program().display());
            for arg in self.get_args() {
                eprint!(" {}", arg.display());
            }
            eprintln!();
        }

        self
    }
}

fn compile_results(
    code: &str,
    mut build: BuildOrRun,
    conf: Configuration,
    verbose: bool,
) -> Result<PathBuf> {
    let [stdout, stderr] = if verbose {
        std::array::from_fn(|_| Stdio::inherit())
    } else {
        std::array::from_fn(|_| Stdio::piped())
    };

    if conf.has_feature(Strings::FEAT_BACKTRACE) {
        build.libs.push(String::from("dw"));
    }

    let has_boehm = conf.has_feature(Strings::FEAT_BOEHM);
    let output = build.out_dir.or(conf.build).unwrap_or_else(|| PathBuf::from("./build"));
    std::fs::create_dir_all(&output)?;

    let file_path = if !build.use_stdin {
        let path = output.join(format!("{}.c", conf.name.as_deref().unwrap_or("main")));
        let mut file = File::create(&path)?;
        print_results(code, build.pretty, verbose, &mut file)?;
        Some(path)
    } else {
        None
    };

    let output = output.join(conf.name.as_deref().unwrap_or("a.out"));
    let debug_flags = ["-fno-omit-frame-pointer", "-rdynamic", "-g"];
    let debug_info = build.debug || build.opt_level.is_none_or(|l| l == 0);
    let mut cc = Command::new(build.cc)
        .args(include_str!("../compile_flags.txt").split("\n").filter(|f| !f.is_empty()))
        .args(build.ccargs.unwrap_or_default().split(' ').filter(|f| !f.is_empty()))
        .arg(format!("-O{}", build.opt_level.unwrap_or_default()))
        .args(debug_info.then_some(debug_flags).into_iter().flatten())
        .args(has_boehm.then_some("-lgc"))
        .args(
            build
                .libs
                .iter()
                .chain(conf.libs.unwrap_or_default().iter())
                .map(|lib| format!("-l{lib}")),
        )
        .args(["-x", "c", "-o"])
        .arg(&output)
        .arg(file_path.as_deref().unwrap_or(Path::new("-")))
        .log_if_verbose(verbose)
        .stdin(Stdio::piped())
        .stdout(stdout)
        .stderr(stderr)
        .spawn()
        .context("Couldn't invoke the compiler")?;

    if file_path.is_none() {
        cc.stdin.take().context("The C compiler closed stdin")?.write_all(code.as_bytes())?;
    }

    let status = cc.wait()?;
    if !status.success() {
        if let Some(mut stderr) = cc.stderr {
            _ = std::io::copy(&mut stderr, &mut std::io::stderr().lock());
        }

        anyhow::bail!(
            "The C compiler returned non-zero exit code {:?}",
            status.code().unwrap_or_default()
        );
    }

    Ok(output)
}

fn print_results(src: &str, pretty: bool, verbose: bool, output: &mut impl Write) -> Result<()> {
    if pretty {
        let mut cc = Command::new("clang-format")
            .log_if_verbose(verbose)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::null())
            .spawn()
            .context("Couldn't invoke clang-format")?;
        cc.stdin.take().context("clang-format closed stdin")?.write_all(src.as_bytes())?;
        let result = cc.wait_with_output()?;
        if !result.status.success() {
            anyhow::bail!(
                "clang-format returned non-zero exit code {:?}",
                result.status.code().unwrap_or_default()
            );
        }

        output.write_all(&result.stdout)?;
    } else {
        output.write_all(src.as_bytes())?;
    }

    Ok(())
}

fn display_diagnostics(diag: &Diagnostics) {
    fn format<'a, S: SourceProvider>(
        provider: &mut S,
        diag: &Diagnostics,
        id: FileId,
        errors: impl IntoIterator<Item = &'a Error>,
        mut format: impl FnMut(&str, Range),
    ) {
        for err in errors.into_iter().filter(|err| err.span.file == id) {
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
        let path = cwd.as_ref().and_then(|cwd| path.strip_prefix(cwd).ok()).unwrap_or(path);
        format(
            &mut provider,
            diag,
            id,
            diag.diagnostics().iter().filter(|e| e.severity.is_error()),
            |msg, range| {
                eprintln!(
                    "{}: {}:{}:{}: {msg}",
                    "error".red(),
                    path.display(),
                    range.start.line + 1,
                    range.start.character + 1
                );
            },
        );
    }

    for (id, path) in diag.paths() {
        let path = cwd.as_ref().and_then(|cwd| path.strip_prefix(cwd).ok()).unwrap_or(path);
        format(
            &mut provider,
            diag,
            id,
            diag.diagnostics().iter().filter(|e| e.severity.is_warning()),
            |msg, range| {
                eprintln!(
                    "{}: {}:{}:{}: {msg}",
                    "warning".yellow(),
                    path.display(),
                    range.start.line + 1,
                    range.start.character + 1
                );
            },
        );
    }
}

fn dump(proj: UnloadedProject, mods: &[String]) -> Result<()> {
    let result = Compiler::new().parse(proj)?;
    result.dump(mods);
    Ok(())
}

fn dump_tokens(input: Option<&Path>) -> Result<()> {
    let source = std::fs::read_to_string(input.context("missing input file")?)?;
    let mut lexer = ctl::Lexer::new(&source, Default::default());
    let mut diag = ctl::Diagnostics::default();
    loop {
        let token = lexer.next(&mut diag);
        if token.data == ctl::Token::Eof {
            break Ok(());
        }

        println!("{token:?}");
    }
}

fn execute_binary(path: &Path, args: Option<Vec<OsString>>, verbose: bool) -> Result<()> {
    let mut cmd = Command::new(path);
    let cmd = cmd.args(args.into_iter().flatten()).log_if_verbose(verbose);

    #[cfg(unix)]
    {
        use std::os::unix::process::CommandExt;
        Err(cmd.exec().into())
    }

    #[cfg(not(unix))]
    {
        let status = cmd.spawn().context("Couldn't invoke the generated program")?.wait()?;
        std::process::exit(status.code().unwrap_or_default());
    }
}

fn main() -> Result<()> {
    let args = Arguments::parse();
    let input = match &args.command {
        SubCommand::Print { input, .. } => input,
        SubCommand::Dump { input, tokens, .. } => {
            if *tokens {
                return dump_tokens(input.as_deref());
            }

            input
        }
        SubCommand::Build { build, .. } => &build.input,
        SubCommand::Run { build, .. } => &build.input,
        SubCommand::Test { build, .. } => &build.input,
        SubCommand::Lsp => {
            tokio::runtime::Builder::new_multi_thread().enable_all().build()?.block_on(async {
                let stdin = tokio::io::stdin();
                let stdout = tokio::io::stdout();
                let (service, socket) = LspService::new(LspBackend::new);
                Server::new(stdin, stdout, socket).serve(service).await
            });
            return Ok(());
        }
    };
    let mut conf = Configuration::default();
    conf.flags.no_bit_int = args.no_bit_int;
    conf.flags.lib = args.shared.unwrap_or(conf.flags.lib);
    conf.flags.minify = args.minify;
    conf.no_std = args.no_std;
    if args.leak {
        conf.remove_feature(Strings::FEAT_BOEHM);
    }

    let mut proj = UnloadedProject::with_conf(input.as_deref().unwrap_or(Path::new(".")), conf)?;
    if let SubCommand::Dump { modules, .. } = &args.command {
        return dump(proj, modules);
    }

    if let SubCommand::Test { .. } | SubCommand::Print { test: true, .. } = &args.command {
        proj.conf.set_feature(Strings::FEAT_TEST);
    }

    let result = Compiler::new().parse(proj)?.typecheck(None).build();
    let (conf, result) = match result {
        (Some(code), conf, diag) => {
            if !args.quiet {
                display_diagnostics(&diag);
            }
            (conf, code)
        }
        (None, _conf, diag) => {
            eprintln!("Compilation failed: ");
            display_diagnostics(&diag);
            std::process::exit(1);
        }
    };
    match args.command {
        SubCommand::Print { output, ugly, .. } => {
            if let Some(output) = output {
                let mut output = File::create(output)?;
                print_results(&result, !ugly, args.verbose, &mut output)?;
            } else {
                print_results(&result, !ugly, args.verbose, &mut std::io::stdout().lock())?;
            }
        }
        SubCommand::Build { build } => _ = compile_results(&result, build, conf, args.verbose)?,
        SubCommand::Test { build } => {
            execute_binary(
                &compile_results(&result, build, conf, args.verbose)?,
                None,
                args.verbose,
            )?;
        }
        SubCommand::Run { build, targs } => {
            execute_binary(
                &compile_results(&result, build, conf, args.verbose)?,
                Some(targs),
                args.verbose,
            )?;
        }
        SubCommand::Dump { .. } | SubCommand::Lsp => {}
    }

    Ok(())
}
