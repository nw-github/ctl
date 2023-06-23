use std::io::{self, Write, BufRead};

use ctl::lexer::Lexer;

fn main() {
    let mut stdout = io::stdout().lock();
    let mut stdin  = io::stdin().lock();
    let mut buffer = String::new();
    loop {
        print!(">> ");
        stdout.flush().unwrap();
        buffer.clear();
        match stdin.read_line(&mut buffer) {
            Ok(0) => break,
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
