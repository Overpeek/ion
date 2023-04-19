use super::{Expr, ToStatic};
use serde::Serialize;

//

#[derive(Debug, Clone, ToStatic, Serialize)]
pub struct BinExpr<'i> {
    pub sides: Box<(Expr<'i>, Expr<'i>)>,
    pub op: BinOp,
}

impl<'i> BinExpr<'i> {
    pub fn new(left: Expr<'i>, op: BinOp, right: Expr<'i>) -> Self {
        Self {
            sides: Box::new((left, right)),
            op,
        }
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
