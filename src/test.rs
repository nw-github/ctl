use anyhow::Context;
use ctl::{Compiler, Diagnostics, FileId, Lexer, Token, UnloadedProject};
use std::{
    io::{Read, Write},
    path::Path,
    process::{Command, Stdio},
    time::Duration,
};
use tempfile::NamedTempFile;
use wait_timeout::ChildExt;

fn test_diagnostics(diag: Diagnostics, expected: &[&str]) -> datatest_stable::Result<()> {
    let mut errors: Vec<_> = diag
        .diagnostics()
        .iter()
        .filter(|e| e.severity.is_error())
        .map(|e| e.message.clone())
        .collect();
    for line in expected {
        if let Some(pos) = errors.iter().position(|err| err.contains(line)) {
            errors.swap_remove(pos);
        } else {
            Err(format!("missing error output: '{line}'"))?;
        }
    }

    if !errors.is_empty() {
        Err(format!("unexpected errors: '{}'", errors.join("\n")))?;
    }

    Ok(())
}

fn compile_test(path: &Path) -> datatest_stable::Result<()> {
    let file = std::fs::read_to_string(path)?;
    let mut diag = Diagnostics::default();
    let mut lexer = Lexer::new(&file, FileId::default());
    let mut errors = vec![];
    let mut expected = vec![];
    while let Token::LineComment(data) = lexer.next(&mut diag).data {
        let data = data.trim();
        let output = data.trim_start_matches("Output:");
        if output != data {
            expected.push(output.trim());
        }

        let output = data.trim_start_matches("Error:");
        if output != data {
            errors.push(output.trim());
        }
    }

    if expected.is_empty() && errors.is_empty() {
        return Err("no requirements specified!".into());
    }

    let (code, _, diag) =
        Compiler::new().parse(UnloadedProject::new(path)?)?.typecheck(Default::default()).build();
    test_diagnostics(diag, &errors)?;
    let Some(code) = code else {
        if !expected.is_empty() {
            return Err(format!("expected '{}', but build failed", expected.join("\n")).into());
        }
        return Ok(());
    };

    let tmpfile = NamedTempFile::new()?.into_temp_path();
    let stdout = {
        let mut cc = Command::new("clang")
            .arg("-o")
            .arg(&tmpfile)
            .args(["-std=c11", "-lgc", "-x", "c", "-"])
            .stdin(Stdio::piped())
            .stdout(Stdio::null())
            .stderr(Stdio::piped())
            .spawn()
            .context("Couldn't invoke the compiler")?;
        cc.stdin.as_mut().context("The C compiler closed stdin")?.write_all(code.as_bytes())?;
        let output = cc.wait_with_output()?;
        if !output.status.success() {
            if let Ok(err) = String::from_utf8(output.stderr) {
                Err(format!(
                    "The C compiler returned non-zero exit code {:?}:\n{err}",
                    output.status.code().unwrap_or_default(),
                ))?;
            } else {
                Err(format!(
                    "The C compiler returned non-zero exit code {:?}",
                    output.status.code().unwrap_or_default(),
                ))?;
            }
        }
        let mut child = Command::new(&tmpfile).stdout(Stdio::piped()).spawn()?;
        let mut stdout = child.stdout.take().unwrap();
        let status = child.wait_timeout(Duration::from_secs(5))?.ok_or("Test took too long!")?;
        if !status.success() {
            Err(format!("binary returned exit code {:?}", status.code()))?;
        }

        let mut data = Vec::new();
        stdout.read_to_end(&mut data)?;
        String::from_utf8(data).context("parsing output of test program")?
    };
    let output: Vec<_> = stdout.trim().split('\n').map(|s| s.trim()).collect();
    if output != expected {
        Err(format!("expected '{}', got '{}'", expected.join("\n"), output.join("\n"),))?;
    }
    Ok(())
}

datatest_stable::harness!(compile_test, "tests", r".*/**/*.ctl");
