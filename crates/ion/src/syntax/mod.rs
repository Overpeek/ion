use std::fmt::{self};

use arcstr::ArcStr;

use crate::util::{PrintSource, Source};

pub use self::{block::*, expr::*, func::*, item::*, stmt::*, var::*};

//

mod block;
mod expr;
mod func;
mod item;
mod stmt;
mod var;

pub mod lexer;

//

/// Source file contents
#[derive(Debug, Clone)]
pub struct Module {
    pub source_file: Option<ArcStr>,
    pub items: Vec<Item>,
}

impl fmt::Display for Source<'_, Module> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if let Some(source_file) = self.inner.source_file.as_ref() {
            writeln!(f, "// !src_file={source_file}\n")?;
        }

        for item in self.inner.items.iter() {
            writeln!(f, "{}\n", item.as_source(self.indent))?;
        }

        Ok(())
    }
}
