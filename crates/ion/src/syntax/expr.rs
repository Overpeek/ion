use std::fmt::{self};

use arcstr::Substr;

use crate::util::{PrintSource, Source};

use super::{FnCall, FnCallExt};

//

#[derive(Debug, Clone)]
pub enum Expr {
    BinExpr { sides: Box<(Expr, Expr)>, op: BinOp },

    Value(Value),

    Variable(Substr),

    FnCall(FnCall),
    FnCallExt(FnCallExt),
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
            Expr::FnCallExt(v) => v.as_source(self.indent).fmt(f),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    Lt,
    Le,
    Eq,
    Ge,
    Gt,
}

impl BinOp {
    pub const fn as_str(self) -> &'static str {
        match self {
            BinOp::Add => "+",
            BinOp::Sub => "-",
            BinOp::Mul => "*",
            BinOp::Div => "/",
            BinOp::Mod => "%",
            BinOp::Lt => "<",
            BinOp::Le => "<=",
            BinOp::Eq => "==",
            BinOp::Ge => ">=",
            BinOp::Gt => ">",
        }
    }
}

impl fmt::Display for Source<'_, BinOp> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.inner.as_str())
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Str(Substr),
    Int(i64),
    Float(f64),
    Bool(bool),
}

impl fmt::Display for Source<'_, Value> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.inner {
            Value::Str(v) => write!(f, "\"{v}\""),
            Value::Int(v) => write!(f, "{v}"),
            Value::Float(v) => write!(f, "{v}"),
            Value::Bool(v) => write!(f, "{v}"),
        }
    }
}
