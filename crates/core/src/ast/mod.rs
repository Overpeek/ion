use ion_macros::ToStatic;
use serde::Serialize;

//

pub mod assign;
pub mod binexpr;
pub mod expr;
pub mod func;
pub mod ident;
pub mod lit;
pub mod path;
pub mod stmt;

pub use assign::*;
pub use binexpr::*;
pub use expr::*;
pub use func::*;
pub use ident::*;
pub use lit::*;
pub use path::*;
pub use stmt::*;

//

#[cfg(target_pointer_width = "64")]
#[allow(non_camel_case_types)]
pub type fsize = f64;
#[cfg(not(target_pointer_width = "64"))]
#[allow(non_camel_case_types)]
pub type fsize = f32;

//

/// Source file contents
#[derive(Debug, Clone, ToStatic, Serialize)]
#[serde(transparent)]
pub struct Module<'i> {
    pub start: Fn<'i>,
}

impl<'i> Module<'i> {
    pub fn from_stmt(stmt: Stmt<'i>) -> Self {
        Self::default().with_stmt(stmt)
    }

    pub fn with_stmt(mut self, stmt: Stmt<'i>) -> Self {
        self.start.block.0 .0.push(stmt);
        self
    }

    pub fn with_stmts(mut self, stmts: impl IntoIterator<Item = Stmt<'i>>) -> Self {
        self.start.block.0 .0.extend(stmts);
        self
    }

    pub fn to_static() {}
}

impl Default for Module<'_> {
    fn default() -> Self {
        Self {
            start: Fn::new().with_name("_start"),
        }
    }
}

// .. and Block and Vec<Statement>
impl<'i> From<Stmts<'i>> for Module<'i> {
    fn from(stmts: Stmts<'i>) -> Self {
        Self::default().with_stmts(stmts)
    }
}

pub trait ToStatic {
    type Static;

    fn to_static(&self) -> Self::Static;
}

impl<T: ToStatic> ToStatic for Vec<T> {
    type Static = Vec<T::Static>;

    fn to_static(&self) -> Self::Static {
        self.into_iter().map(|item| item.to_static()).collect()
    }
}

impl<T: ToStatic> ToStatic for Box<T> {
    type Static = Box<T::Static>;

    fn to_static(&self) -> Self::Static {
        Box::new(self.as_ref().to_static())
    }
}

impl<A: ToStatic, B: ToStatic> ToStatic for (A, B) {
    type Static = (A::Static, B::Static);

    fn to_static(&self) -> Self::Static {
        (self.0.to_static(), self.1.to_static())
    }
}

impl ToStatic for usize {
    type Static = Self;

    fn to_static(&self) -> Self::Static {
        *self
    }
}
