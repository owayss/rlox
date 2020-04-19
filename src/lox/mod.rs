use std::fs;
use std::io::{self, Write};
use std::path::Path;

mod expr;
mod parser;
mod scanner;
mod token;

pub fn run_file(filepath: &str) {
    run(&fs::read_to_string(Path::new(filepath)).unwrap());
}
pub fn run_repl() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    loop {
        stdout.write("> ".as_bytes()).unwrap();
        stdout.flush().unwrap();

        let mut buf = String::new();
        stdin.read_line(&mut buf).unwrap();
        run(&buf);
    }
}
fn run(source: &str) {
    let mut s = scanner::Scanner::new(source);
    let tokens = s.scan_tokens();
    let mut p = parser::Parser::new(tokens);
    if let Ok(e) = p.parse() {
        println!("{}", e);
    }
}

fn error(line: usize, msg: &str) {
    report(line, "", msg);
}
fn report(line: usize, pos: &str, msg: &str) {
    eprintln!("[line {}] Error {}: {}", line, pos, msg);
}
