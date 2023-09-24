use std::fmt;

use serde::Serialize;

use super::{Expr, ToStatic};
use crate::ty::IonType;

//

#[derive(Debug, Clone, ToStatic, Serialize)]
pub struct BinExpr<'i> {
    pub ty: IonType,
    pub sides: Box<(Expr<'i>, Expr<'i>)>,
    pub op: BinOp,
}

impl<'i> BinExpr<'i> {
    pub fn new(left: Expr<'i>, op: BinOp, right: Expr<'i>) -> Self {
        Self {
            ty: <_>::default(),
            sides: Box::new((left, right)),
            op,
        }
    }

    pub fn code(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        self.sides.0.code(f, indent)?;
        write!(f, " ")?;
        self.op.code(f, indent)?;
        write!(f, " ")?;
        self.sides.1.code(f, indent)
    }
}

#[derive(Debug, Clone, Copy, ToStatic, Serialize)]
#[to_static(result = "Self")]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl BinOp {
    pub fn code(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinOp::Add => '+',
                BinOp::Sub => '-',
                BinOp::Mul => '*',
                BinOp::Div => '/',
            }
        )
    }
}
