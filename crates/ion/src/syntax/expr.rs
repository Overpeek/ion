use std::fmt::{self};

use arcstr::Substr;

use crate::util::{PrintSource, Source};

use super::FnCall;

//

#[derive(Debug, Clone)]
pub enum Expr {
    BinExpr { sides: Box<(Expr, Expr)>, op: BinOp },

    Value(Value),

    Variable(Substr),

    FnCall(FnCall),
}

impl fmt::Display for Source<'_, Expr> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.inner {
            Expr::BinExpr { sides, op } => {
                let lhs = sides.0.as_source(self.indent);
                let rhs = sides.1.as_source(self.indent);
                let op = op.as_source(self.indent);
                write!(f, "({lhs} {op} {rhs})")
            }
            Expr::Value(v) => v.as_source(self.indent).fmt(f),
            Expr::Variable(v) => write!(f, "{v}"),
            Expr::FnCall(v) => v.as_source(self.indent).fmt(f),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl fmt::Display for Source<'_, BinOp> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.inner {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::Mul => write!(f, "*"),
            BinOp::Div => write!(f, "/"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
}

impl fmt::Display for Source<'_, Value> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.inner {
            Value::Int(v) => write!(f, "{v}"),
            Value::Float(v) => write!(f, "{v}"),
            Value::Bool(v) => write!(f, "{v}"),
        }
    }
}
