use std::{
    ffi::OsString,
    io::Write,
    path::Path,
    process::{Command, Stdio},
    time::Duration,
};

use anyhow::Context;
use tempfile::NamedTempFile;
use wait_timeout::ChildExt as _;

use ctl::{Compiler, TestArgs};

#[test]
fn run_unit_tests() -> anyhow::Result<()> {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("unit_tests");
    let (code, conf, diag) = Compiler::new()
        .parse(&path, Default::default())?
        .modify_conf(|conf| {
            conf.test_args = Some(TestArgs {
                test: None,
                modules: Some(vec!["std".into(), "unit_tests".into()]),
            })
        })
        .typecheck(None)
        .build();
    let Some(code) = code else {
        for err in diag.diagnostics() {
            if err.severity.is_error() {
                eprintln!("{}", err.message);
            }
        }

        anyhow::bail!("build failed");
    };

    let tmpfile = NamedTempFile::new()?.into_temp_path();
    let mut cc = Command::new("clang")
        .arg("-o")
        .arg(&tmpfile)
        .args(["-std=c11", "-x", "c", "-"])
        .args(conf.libs.into_iter().map(|lib| match lib {
            ctl::package::Lib::Name(name) => OsString::from(format!("-l{name}")),
            ctl::package::Lib::Path(path) => path.into_os_string(),
        }))
        .stdin(Stdio::piped())
        .stdout(Stdio::null())
        .stderr(Stdio::piped())
        .spawn()
        .context("Couldn't invoke the compiler")?;
    cc.stdin.as_mut().context("The C compiler closed stdin")?.write_all(code.as_bytes())?;

    let output = cc.wait_with_output()?;
    if !output.status.success() {
        if let Ok(err) = String::from_utf8(output.stderr) {
            anyhow::bail!(
                "The C compiler returned non-zero exit code {:?}:\n{err}",
                output.status.code().unwrap_or_default(),
            );
        } else {
            anyhow::bail!(
                "The C compiler returned non-zero exit code {:?}",
                output.status.code().unwrap_or_default(),
            );
        }
    }
    let mut child = Command::new(&tmpfile).spawn()?;
    let status = child.wait_timeout(Duration::from_secs(5))?.context("tests timed out")?;
    if !status.success() {
        anyhow::bail!("binary returned exit code {:?}", status.code());
    }

    Ok(())
}
