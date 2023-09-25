use std::fmt;

use crate::util::{PrintSource, Source};

use super::{Expr, FnCall, FnCallExt, Let};

//

#[derive(Debug, Clone)]
pub enum Stmt {
    Return(Return),
    Let(Let),
    FnCall(FnCall),
    FnCallExt(FnCallExt),
    // Expr(Expr),
}

impl fmt::Display for Source<'_, Stmt> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.inner {
            Stmt::Return(v) => v.as_source(self.indent).fmt(f),
            Stmt::Let(v) => v.as_source(self.indent).fmt(f),
            Stmt::FnCall(v) => v.as_source(self.indent).fmt(f),
            Stmt::FnCallExt(v) => v.as_source(self.indent).fmt(f),
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
