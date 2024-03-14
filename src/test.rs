use anyhow::Context;
use ctl::{project_from_file, CodegenFlags, Compiler, Diagnostics, FileId, Lexer, Token};
use std::{
    io::Write,
    path::Path,
    process::{Command, Stdio},
};
use tempfile::NamedTempFile;

fn test_diagnostics(diag: Diagnostics, expected: &[&str]) -> datatest_stable::Result<()> {
    for line in expected {
        if !diag.errors().iter().any(|err| err.message.contains(line)) {
            return Err(format!("missing error output: '{line}'").into());
        }
    }

    Ok(())
}

fn compile_test(path: &Path) -> datatest_stable::Result<()> {
    let file = std::fs::read_to_string(path)?;
    let mut diag = Diagnostics::default();
    let mut lexer = Lexer::new(&file, FileId::default());

    let mut errors = vec![];
    let mut expected = String::new();
    while let Token::LineComment(data) = lexer.next(&mut diag).data {
        let data = data.trim();
        let output = data.trim_start_matches("Output:");
        if output != data {
            expected.push_str(output.trim());
        }

        let output = data.trim_start_matches("Error:");
        if output != data {
            errors.push(output.trim());
        }
    }

    let proj = project_from_file(path, vec![], false, false);
    let compiler = Compiler::new()
        .parse(proj)?
        .typecheck(Default::default())
        .build(CodegenFlags::default());
    match compiler {
        Ok((diag, code)) => {
            test_diagnostics(diag, &errors)?;

            let tmpfile = NamedTempFile::new()?.into_temp_path();
            let output = {
                let mut cc = Command::new("clang")
                    .arg("-o")
                    .arg(&tmpfile)
                    .arg("-std=c11")
                    .arg("-lgc")
                    .args(["-x", "c", "-"])
                    .stdin(Stdio::piped())
                    .stdout(Stdio::null())
                    .stderr(Stdio::piped())
                    .spawn()
                    .context("Couldn't invoke the compiler")?;
                cc.stdin
                    .as_mut()
                    .context("The C compiler closed stdin")?
                    .write_all(code.as_bytes())?;
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
                let result = Command::new(&tmpfile)
                    .stdout(Stdio::piped())
                    .spawn()?
                    .wait_with_output()?;
                if !result.status.success() {
                    Err(format!(
                        "binary returned exit code {:?}",
                        result.status.code()
                    ))?;
                }

                String::from_utf8(result.stdout).context("parsing output of test program")?
            };
            let output = output.trim();

            if !output.contains(&expected) {
                Err(format!(
                    "output didn't match! expected '{expected}', got '{output}'"
                ))?;
            }
            Ok(())
        }
        Err(diag) => test_diagnostics(diag, &errors),
    }
}

datatest_stable::harness!(compile_test, "dt", r".*/**/*.ctl");
