use super::expr::Expr;
use super::stmt::Stmt;
use std::cmp::Eq;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(PartialEq)]
pub struct RefExpr(Rc<Expr>);
impl Eq for RefExpr {}
impl Hash for RefExpr {
    fn hash<H: Hasher>(&self, hasher: &mut H) {
        (&*self.0 as *const Expr).hash(hasher);
    }
}

pub type SideTable = HashMap<RefExpr, usize>;

pub struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
}
impl Resolver {
    fn new() -> Self {
        Resolver { scopes: Vec::new() }
    }
    fn resolve(&self, stmts: &Vec<Stmt>, locals: SideTable) {}
}
#[cfg(test)]
mod tests {
    use super::{
        super::{interpreter, parser, scanner},
        SideTable,
    };
    #[test]
    fn test_resolve() {
        let semantic_bug = r#"
        var x = "global";
        {
            fn showX() {
                print x;
            }
            showX();
            var x = "block";
            showX();
        }
        "#;
        let expected = "global\nglobal\n";
        let mut s = scanner::Scanner::new(&semantic_bug);
        let tokens = s.scan_tokens();
        let mut p = parser::Parser::new(tokens);
        let mut out = Vec::<u8>::new();
        interpreter::Interpreter::new(&mut out, None)
            .interpret(p.parse().unwrap())
            .unwrap();
        println!("{}", std::str::from_utf8(&out).unwrap());
        assert_eq!(expected, std::str::from_utf8(&out).unwrap())
    }
}
