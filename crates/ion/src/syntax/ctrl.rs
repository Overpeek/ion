use std::fmt::{self, Display};

use crate::util::{PrintSource, Source};

use super::{Block, Expr};

//

#[derive(Debug, Clone)]
pub struct CtrlIf {
    pub condition: Box<Expr>,
    pub block: Block,
}

impl Display for Source<'_, CtrlIf> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let CtrlIf { condition, block } = self.inner;
        let condition = condition.as_source(self.indent);
        let block = block.as_source(self.indent + 4);

        write!(f, "if ({condition}) {block}")
    }
}
