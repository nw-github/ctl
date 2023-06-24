use std::{
    env,
    io::{self, BufRead, Write},
};

use ctl::{lexer::Lexer, parser::Parser, pretty};

fn main() -> anyhow::Result<()> {
    if let Some(file) = env::args_os().nth(1) {
        let file = std::fs::read_to_string(file)?;
        let stmts = Parser::new(&file).parse().unwrap();

        pretty::dump_ast(&stmts, 0);

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
            }
            _ => {}
        }
    }
}
