use std::fs;
use std::io::{self, Write};
use std::path::Path;

mod callable;
mod environment;
mod expr;
mod interpreter;
mod parser;
mod resolver;
mod scanner;
mod stmt;
mod token;

#[derive(Debug)]
pub enum LoxErr {
    RuntimeErr(interpreter::RuntimeErr),
    ParseErr(parser::ParseErr),
    ResolveErr(resolver::ResolveErr),
}

pub fn run_file(filepath: &str) {
    run(
        &mut interpreter::Interpreter::new(&mut std::io::stdout(), None),
        &fs::read_to_string(Path::new(filepath)).unwrap(),
    )
    .unwrap();
}
pub fn run_repl() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut interpreter = interpreter::Interpreter::new(&mut stdout, None);
    loop {
        io::stdout().write("> ".as_bytes()).unwrap();
        io::stdout().flush().unwrap();

        let mut buf = String::new();
        stdin.read_line(&mut buf).unwrap();

        match run(&mut interpreter, &buf) {
            Err(e) => eprintln!("{:?}", e),
            Ok(val) => {
                if let Some(val) = val {
                    println!("{}", val)
                }
            }
        }
    }
}
fn run(
    i: &mut interpreter::Interpreter,
    source: &str,
) -> Result<Option<interpreter::Value>, LoxErr> {
    let mut s = scanner::Scanner::new(source);
    let tokens = s.scan_tokens();
    let mut p = parser::Parser::new(tokens);
    let ret = p.parse();
    if let Err(e) = ret {
        return Err(LoxErr::ParseErr(e));
    }
    let ret = ret.unwrap();
    let stmts: Vec<&stmt::Stmt> = ret.iter().map(|s| s).collect();
    let mut resolver = resolver::Resolver::new();
    let ret = resolver.resolve(&stmts);
    if let Err(e) = ret {
        return Err(LoxErr::ResolveErr(e));
    }
    let locals = ret.unwrap();
    i.locals = locals;
    match i.interpret(stmts) {
        Ok(val) => Ok(val),
        Err(e) => Err(LoxErr::RuntimeErr(e)),
    }
}

fn error(line: usize, msg: &str) {
    report(line, "", msg);
}
fn report(line: usize, pos: &str, msg: &str) {
    eprintln!("[line {}] Error {}: {}", line, pos, msg);
}

#[cfg(test)]
mod tests {
    use super::run;
    #[test]
    fn test_interpreter() {
        let mut stdout = std::io::stdout();
        let mut interpreter = super::interpreter::Interpreter::new(&mut stdout, None);
        let fib = |n: usize| -> String {
            let body = "
        fn fib(n) {
            if (n == 0 or n == 1) {
                return n;
            }
            else {
                return fib(n-1) + fib(n-2);
            }
        }";
            format!("{} fib({});", body, n)
        };
        struct Case {
            input: usize,
            expected: usize,
        }
        let battery: [Case; 3] = [
            Case {
                input: 0,
                expected: 0,
            },
            Case {
                input: 1,
                expected: 1,
            },
            Case {
                input: 6,
                expected: 8,
            },
        ];
        for case in &battery {
            let val = run(&mut interpreter, &fib(case.input)).unwrap().unwrap();
            if let super::interpreter::Value::Number(out) = val {
                assert_eq!(out as usize, case.expected);
            }
        }
    }
}
