use std::fmt;

use crate::util::{PrintSource, Source};

use super::{Expr, FnCall, Let};

//

#[derive(Debug, Clone)]
pub enum Stmt {
    Return(Return),
    Let(Let),
    FnCall(FnCall),
    // Expr(Expr),
}

impl fmt::Display for Source<'_, Stmt> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.inner {
            Stmt::Return(v) => v.as_source(self.indent).fmt(f),
            Stmt::Let(v) => v.as_source(self.indent).fmt(f),
            Stmt::FnCall(v) => v.as_source(self.indent).fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Return(pub Expr);

impl fmt::Display for Source<'_, Return> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let expr = self.inner.0.as_source(self.indent);
        write!(f, "return {expr}")
    }
}
