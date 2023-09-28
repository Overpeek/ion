use std::fmt;

use arcstr::Substr;

use crate::util::{PrintSource, Source};

use super::{Block, Expr};

//

#[derive(Debug, Clone)]
pub struct CtrlIf {
    pub condition: Box<Expr>,
    pub block: Block,
}

impl fmt::Display for Source<'_, CtrlIf> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let CtrlIf { condition, block } = self.inner;
        let condition = condition.as_source(self.indent);
        let block = block.as_source(self.indent + 4);

        write!(f, "if ({condition}) {block}")
    }
}

#[derive(Debug, Clone)]
pub struct CtrlFor {
    pub id: Substr,
    pub range: Range,
    pub block: Block,
}

impl fmt::Display for Source<'_, CtrlFor> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let CtrlFor { id, range, block } = self.inner;
        let range = range.as_source(self.indent);
        let block = block.as_source(self.indent + 4);

        write!(f, "for {id} in {range} {block}")
    }
}

#[derive(Debug, Clone)]
pub struct Range {
    pub range: Box<(Expr, Expr)>,
    pub kind: RangeKind,
}

#[derive(Debug, Clone)]
pub enum RangeKind {
    Closed,
    Open,
}

impl fmt::Display for Source<'_, Range> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Range { range, kind } = self.inner;
        let lhs = range.0.as_source(self.indent);
        let rhs = range.1.as_source(self.indent);

        match kind {
            RangeKind::Closed => write!(f, "{lhs}..{rhs}"),
            RangeKind::Open => write!(f, "{lhs}..={rhs}"),
        }
    }
}
