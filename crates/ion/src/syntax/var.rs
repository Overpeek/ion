use std::fmt;

use arcstr::Substr;

use crate::util::{PrintSource, Source};

use super::{BinOp, Expr};

//

#[derive(Debug, Clone)]
pub struct Let {
    pub id: Substr,
    pub expr: Expr,
}

impl fmt::Display for Source<'_, Let> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Let { id, expr } = self.inner;
        let expr = expr.as_source(self.indent);

        write!(f, "let {id} = {expr}")
    }
}

#[derive(Debug, Clone)]
pub struct Assign {
    pub id: Substr,
    pub expr: Expr,
    pub op: AssignOp,
}

impl fmt::Display for Source<'_, Assign> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Assign { id, expr, op } = self.inner;
        let expr = expr.as_source(self.indent);

        write!(f, "{id} {op} {expr}")
    }
}

#[derive(Debug, Clone, Copy)]
pub enum AssignOp {
    Assign,
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

impl AssignOp {
    pub const fn as_binop(self) -> Option<BinOp> {
        match self {
            Self::Assign => None,
            Self::Add => Some(BinOp::Add),
            Self::Sub => Some(BinOp::Sub),
            Self::Mul => Some(BinOp::Mul),
            Self::Div => Some(BinOp::Div),
            Self::Mod => Some(BinOp::Mod),
        }
    }
}

impl fmt::Display for AssignOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                AssignOp::Assign => "=",
                AssignOp::Add => "+=",
                AssignOp::Sub => "-=",
                AssignOp::Mul => "*=",
                AssignOp::Div => "/=",
                AssignOp::Mod => "%=",
            }
        )
    }
}
