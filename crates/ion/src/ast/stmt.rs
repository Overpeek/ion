use std::{
    fmt,
    ops::{Deref, DerefMut},
};

use serde::Serialize;

use super::{Assign, Expr, Return, ReturnVoid, ToStatic};
use crate::ty::IonType;

//

/// A group of 0 or more statements
#[derive(Debug, Clone, ToStatic, Default, Serialize)]
pub struct Stmts<'i>(pub Vec<Stmt<'i>>);

impl Stmts<'_> {
    pub fn code(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        for stmt in self.iter() {
            write!(f, "{:indent$}", "")?;
            stmt.code(f, indent)?;
            writeln!(f)?;
        }
        Ok(())
    }
}

impl<'i> Deref for Stmts<'i> {
    type Target = Vec<Stmt<'i>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Stmts<'_> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl IntoIterator for Stmts<'_> {
    type Item = <<Self as Deref>::Target as IntoIterator>::Item;
    type IntoIter = <<Self as Deref>::Target as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

/// 1 line of code or a part of a line separated by `;`
#[derive(Debug, Clone, ToStatic, Serialize)]
// #[serde(transparent)]
// #[serde(untagged)]
pub enum Stmt<'i> {
    /// An assignment of an [`Expression`] to a name
    Assign(Assign<'i>),

    /// An expression
    ///
    /// [`Expression`]
    Expr(Expr<'i>),

    /* /// A function call
    ///
    /// ```text
    /// a(params)
    /// ```
    FnCall(FnCall<'i>), */

    /* /// A named function
    ///
    /// ```text
    /// fn a(params) {}
    /// ```
    ///
    /// this is syntax sugar for:
    ///
    /// ```text
    /// a = fn(params) {}
    /// ```
    Fn(Fn<'i>), */
    /// A return from the function
    ///
    /// ```text
    /// return 4;
    /// ```
    Return(Return<'i>),

    /// A void return from the function
    ///
    /// ```text
    /// return;
    /// ```
    ReturnVoid(ReturnVoid),

    /// A conditional branch
    ///
    /// ```text
    /// if a {}
    /// else if b {} // optional
    /// else {}        // optional
    /// ```
    Conditional(),

    /// A loop over an iterator
    ///
    /// ```text
    /// for a in range(0, 100) {
    ///     // code to be repeated
    /// }
    /// ```
    IteratorLoop(),

    /// A while loop
    ///
    /// ```text
    /// while true {
    ///     // code to be repeated
    /// }
    /// ```
    Loop(),
}

impl<'i> Stmt<'i> {
    pub fn new_return(e: Option<Expr<'i>>) -> Self {
        if let Some(e) = e {
            Self::Return(Return { value: e })
        } else {
            Self::ReturnVoid(ReturnVoid)
        }
    }

    pub fn code(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        match self {
            Stmt::Assign(v) => v.code(f, indent),
            Stmt::Expr(v) => v.code(f, indent),
            Stmt::Return(v) => v.code(f, indent),
            Stmt::ReturnVoid(v) => v.code(f, indent),
            Stmt::Conditional() => todo!(),
            Stmt::IteratorLoop() => todo!(),
            Stmt::Loop() => todo!(),
        }
    }
}

/// A group of statements surrounded by `{` and `}`
#[derive(Debug, Clone, ToStatic, Default, Serialize)]
pub struct Block<'i> {
    pub ty: IonType,
    pub stmts: Stmts<'i>,
}

impl Block<'_> {
    pub fn code(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        writeln!(f, "{{")?;
        self.stmts.code(f, indent + 4)?;
        write!(f, "{:indent$}}}", "")
    }
}

impl<'i> From<Stmts<'i>> for Block<'i> {
    fn from(stmts: Stmts<'i>) -> Self {
        Self {
            ty: <_>::default(),
            stmts,
        }
    }
}
