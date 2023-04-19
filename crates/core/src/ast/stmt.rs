use super::{Assign, Expr, Fn, FnCall, Return, ReturnVoid, ToStatic};
use serde::Serialize;
use std::ops::{Deref, DerefMut};

//

/// A group of 0 or more statements
#[derive(Debug, Clone, ToStatic, Default, Serialize)]
pub struct Stmts<'i>(pub Vec<Stmt<'i>>);

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

    /* /// An expression
    ///
    /// [`Expression`]
    Expr(Expr<'i>), */
    /// A function call
    ///
    /// ```text
    /// a(params)
    /// ```
    FnCall(FnCall<'i>),

    /// A named function
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
    Fn(Fn<'i>),

    /// A return from the function
    ///
    /// ```text
    /// return 4 + 4;
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
}

/// A group of statements surrounded by `{` and `}`
#[derive(Debug, Clone, ToStatic, Default, Serialize)]
pub struct Block<'i>(pub Stmts<'i>);

impl<'i> From<Stmts<'i>> for Block<'i> {
    fn from(value: Stmts<'i>) -> Self {
        Self(value)
    }
}
