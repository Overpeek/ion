use std::fmt;

use arcstr::Substr;

use crate::util::{PrintSource, Source};

use super::Expr;

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
