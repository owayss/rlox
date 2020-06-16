use std::fs;
use std::io::{self, Write};
use std::path::Path;

mod environment;
mod expr;
mod interpreter;
mod parser;
mod scanner;
mod stmt;
mod token;

pub fn run_file(filepath: &str) {
    run(
        &mut interpreter::Interpreter::new(),
        &fs::read_to_string(Path::new(filepath)).unwrap(),
    );
}
pub fn run_repl() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut interpreter = interpreter::Interpreter::new();
    loop {
        stdout.write("> ".as_bytes()).unwrap();
        stdout.flush().unwrap();

        let mut buf = String::new();
        stdin.read_line(&mut buf).unwrap();
        run(&mut interpreter, &buf);
    }
}
fn run(i: &mut interpreter::Interpreter, source: &str) {
    let mut s = scanner::Scanner::new(source);
    let tokens = s.scan_tokens();
    let mut p = parser::Parser::new(tokens);
    if let Ok(stmts) = p.parse() {
        if let Err(err) = i.interpret(stmts) {
            eprintln!("{:#?}", err)
        }
    }
}

fn error(line: usize, msg: &str) {
    report(line, "", msg);
}
fn report(line: usize, pos: &str, msg: &str) {
    eprintln!("[line {}] Error {}: {}", line, pos, msg);
}
