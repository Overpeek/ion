use std::fmt;

use crate::util::{PrintSource, Source};

use super::FnDef;

//

#[derive(Debug, Clone)]
pub enum Item {
    FnDef(FnDef),
}

impl fmt::Display for Source<'_, Item> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.inner {
            Item::FnDef(i) => {
                write!(f, "{}", i.as_source(self.indent))
            }
        }
    }
}
