use std::fmt;

use crate::util::{Padding, PrintSource, Source};

use super::Stmt;

//

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

impl fmt::Display for Source<'_, Block> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let pad = Padding(self.indent);

        for stmt in self.inner.stmts.iter() {
            let stmt = stmt.as_source(self.indent);
            writeln!(f, "{pad}{stmt};")?;
        }

        Ok(())
    }
}
