use std::fmt;

use crate::util::{PrintSource, Source};

use super::{Assign, CtrlIf, Expr, Let};

//

#[derive(Debug, Clone)]
pub enum Stmt {
    Return(Return),
    Let(Let),
    Assign(Assign),
    CtrlIf(CtrlIf),
    Expr(Expr),
    Semi,
}

impl fmt::Display for Source<'_, Stmt> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.inner {
            Stmt::Return(v) => v.as_source(self.indent).fmt(f),
            Stmt::Let(v) => v.as_source(self.indent).fmt(f),
            Stmt::Assign(v) => v.as_source(self.indent).fmt(f),
            Stmt::CtrlIf(v) => v.as_source(self.indent).fmt(f),
            Stmt::Expr(v) => v.as_source(self.indent).fmt(f),
            _ => Ok(()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Return(pub Option<Expr>);

impl fmt::Display for Source<'_, Return> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(expr) = self.inner.0.as_ref() {
            let expr = expr.as_source(self.indent);
            write!(f, "return {expr}")
        } else {
            write!(f, "return")
        }
    }
}
