use super::expr::Expr;
use super::stmt::{FnDeclaration, Stmt};
use std::collections::HashMap;

#[derive(Debug)]
pub enum ResolveErr {
    LocalVarReferencedInOwnInitializer,
}

type Position = usize;
pub type VarRef = (String, Position);
pub type SideTable = HashMap<VarRef, usize>;

enum VarState {
    Declared,
    Defined,
}

pub struct Resolver {
    scopes: Vec<HashMap<String, VarState>>,
    locals: SideTable,
}
impl Resolver {
    pub fn new() -> Self {
        Resolver {
            scopes: vec![],
            locals: SideTable::new(),
        }
    }

    pub fn resolve(&mut self, stmts: &Vec<&Stmt>) -> Result<SideTable, ResolveErr> {
        for stmt in stmts {
            self.resolve_stmt(stmt)?;
        }
        Ok(self.locals.clone())
    }

    pub fn resolve_stmt(&mut self, stmt: &Stmt) -> Result<(), ResolveErr> {
        match stmt {
            Stmt::Block(stmts) => {
                self.begin_scope();
                self.resolve(&stmts.iter().map(|s| s).collect())?;
                self.end_scope();
            }
            Stmt::Var(t, e) => {
                self.declare(&t.lexeme);
                self.resolve_expr(e)?;
                self.define(&t.lexeme);
            }
            Stmt::Fn(declaration) => {
                self.begin_scope();
                self.declare(&declaration.name.lexeme);
                self.define(&declaration.name.lexeme);
                self.resolve_fn(declaration)?;
                self.end_scope();
            }
            Stmt::If(e, then_branch, else_branch) => {
                self.resolve_expr(e)?;
                self.resolve_stmt(then_branch)?;
                if let Some(else_branch) = else_branch {
                    self.resolve_stmt(else_branch)?;
                }
            }
            Stmt::While(e, body) => {
                self.resolve_expr(e)?;
                self.resolve_stmt(body)?;
            }
            Stmt::Expr(e) | Stmt::Print(e) | Stmt::Return(e) => self.resolve_expr(e)?,
        }
        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.insert(0, HashMap::<String, VarState>::new());
    }
    fn end_scope(&mut self) {
        if !self.scopes.is_empty() {
            self.scopes.remove(0);
        }
    }
    fn declare(&mut self, name: &str) {
        if let Some(scope) = self.scopes.first_mut() {
            scope.insert(name.to_owned(), VarState::Declared);
        }
    }
    fn define(&mut self, name: &str) {
        if let Some(scope) = self.scopes.first_mut() {
            if let Some(var) = scope.get_mut(name) {
                *var = VarState::Defined;
            }
        }
    }
    fn resolve_expr(&mut self, e: &Expr) -> Result<(), ResolveErr> {
        match e {
            Expr::Variable(t) => {
                if !self.scopes.is_empty() {
                    if let Some(VarState::Declared) = self.scopes.first().unwrap().get(&t.lexeme) {
                        return Err(ResolveErr::LocalVarReferencedInOwnInitializer);
                    }
                }
                self.resolve_local_expr(e, &t.lexeme)
            }
            Expr::Assignment(t, assigned) => {
                self.resolve_expr(assigned)?;
                self.resolve_local_expr(assigned, &t.lexeme)
            }
            Expr::Binary(_, left, right) | Expr::Logical(_, left, right) => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
            }
            Expr::Call(callee, _, args) => {
                self.resolve_expr(callee)?;
                for arg in args {
                    self.resolve_expr(arg)?;
                }
            }
            Expr::Unary(_, e) | Expr::Grouping(e) => {
                self.resolve_expr(e)?;
            }
            _ => {}
        }
        Ok(())
    }

    fn resolve_local_expr(&mut self, e: &Expr, name: &str) {
        for (hops, scope) in self.scopes.iter().enumerate().rev() {
            if scope.contains_key(name) {
                if let Expr::Variable(t) = e {
                    self.locals
                        .insert((t.lexeme.to_owned(), t.pos_in_src()), hops);
                }
            }
        }
    }
    fn resolve_fn(&mut self, declaration: &FnDeclaration) -> Result<(), ResolveErr> {
        self.begin_scope();
        for param in &declaration.params {
            self.declare(param);
            self.define(param);
        }
        self.resolve_stmt(&declaration.body)?;
        self.end_scope();
        Ok(())
    }
}
#[cfg(test)]
mod tests {
    use super::super::{interpreter, parser, scanner};
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
            .interpret(p.parse().unwrap().iter().map(|x| x).collect())
            .unwrap();
        println!("{}", std::str::from_utf8(&out).unwrap());
        assert_eq!(expected, std::str::from_utf8(&out).unwrap())
    }
}
