mod lox;
use std::env;
fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        1 => lox::run_repl(),
        2 => lox::run_file(&args[1]),
        _ => {
            eprintln!("Usage: {} [script]", args[0]);
        }
    }
}
