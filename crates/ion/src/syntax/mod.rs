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
    pub src_files: Vec<ArcStr>,
    pub items: Vec<Item>,
}

impl Module {
    pub fn extend(&mut self, rhs: Module) -> &mut Self {
        self.src_files.extend(rhs.src_files);
        self.items.extend(rhs.items);
        self
    }
}

impl fmt::Display for Source<'_, Module> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Module { src_files, items } = self.inner;

        writeln!(f, "// !src_file(s)={src_files:?}\n")?;

        for item in items.iter() {
            writeln!(f, "{}\n", item.as_source(self.indent))?;
        }

        Ok(())
    }
}
