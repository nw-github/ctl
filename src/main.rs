use std::{
    env,
    io::{self, BufRead, Write},
};

use ctl::{lexer::Lexer, parser::Parser, pretty};

fn main() -> anyhow::Result<()> {
    if let Some(file) = env::args_os().nth(1) {
        let buffer = std::fs::read_to_string(file)?;
        match Parser::new(&buffer).parse() {
            Ok(module) => pretty::print_stmt(&module, 0),
            Err(errors) => eprintln!("{errors:?}"),
        }

        return Ok(());
    }

    let mut stdout = io::stdout().lock();
    let mut stdin = io::stdin().lock();
    let mut buffer = String::new();
    loop {
        print!(">> ");
        stdout.flush().unwrap();
        buffer.clear();
        match stdin.read_line(&mut buffer) {
            Ok(0) => break Ok(()),
            Ok(len) => {
                let buffer = &buffer[..len];
                for token in Lexer::new(buffer) {
                    let token = token.unwrap();
                    println!("{:?} '{}'", token.data, token.span.text(buffer));
                }

                match Parser::new(buffer).parse() {
                    Ok(module) => pretty::print_stmt(&module, 0),
                    Err(errors) => eprintln!("{errors:?}"),
                }
            }
            _ => {}
        }
    }
}
