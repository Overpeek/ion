use std::fmt;

use serde::Serialize;

use super::{BinExpr, Fn, FnCall, Literal, Path, ToStatic};

//

#[derive(Debug, Clone, Serialize)]
pub enum Expr<'i> {
    /// # A nameless function (or lambda or closure)
    ///
    /// ```text
    /// function(params) {}
    /// ```
    NamelessFn(Fn<'i>),
    // NamelessFnRef(u32),
    /// A function call
    ///
    /// ```text
    /// a(params)
    /// ```
    FnCall(FnCall<'i>),

    /// # A unary (prefix) operation
    ///
    /// ```text
    /// - a
    /// ```
    /*///
    /// ## Functions can be used as operators
    ///
    /// The first parameter gets the value
    ///
    /// If the function took no parameters, an error is generated
    ///
    /// If all parameters are used, the function is executed immediately with the given parameters
    ///
    /// If there are parameters left, a new function is created with the first parameter being
    /// bound
    ///
    /// ### Single parameter functions can be used as operators
    ///
    /// ```text
    /// function neg(a) { return -a }
    ///
    /// neg 2 // same as `neg(2)`
    /// ```
    ///
    /// ### n>=2 parameter functions can be bound
    ///
    /// ```text
    /// function add(a, b) { return a + b }
    ///
    /// add 1 2 // same as `(add 1) 2` which is the same as `add(1, 2)`
    /// ```
    ///
    /// ### If there are parameters left, a new function is created
    ///
    /// ```text
    /// function add(a, b) { return a + b }
    ///
    /// add_2 = add 2
    ///
    /// add_2 5 // same as `add(2, 5)`
    /// ```*/
    UnExpr(),

    /// # A binary (infix) operation
    ///
    /// ```text
    /// a + b
    /// ```
    /*///
    /// ## Functions can be used as operators
    ///
    /// The first parameter gets the left side and the second parameter gets the right side*/
    BinExpr(BinExpr<'i>),

    /// A hardcoded literal
    ///
    /// ```text
    /// "a"
    /// ```
    ///
    /// ```text
    /// 100_000
    /// ```
    Literal(Literal<'i>),

    /// A path to read data from
    ///
    /// ´´´text
    /// c
    /// ´´´
    ///
    /// ´´´text
    /// a.b.c
    /// ´´´
    Path(Path<'i>),
}

impl Expr<'_> {
    pub fn code(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        match self {
            Expr::NamelessFn(v) => v.code(f, indent),
            Expr::FnCall(v) => v.code(f, indent),
            Expr::UnExpr() => todo!(),
            Expr::BinExpr(v) => v.code(f, indent),
            Expr::Literal(v) => v.code(f, indent),
            Expr::Path(v) => v.code(f, indent),
        }
    }
}

impl ToStatic for Expr<'_> {
    type Static = Expr<'static>;

    fn to_static(&self) -> Self::Static {
        match self {
            Expr::NamelessFn(v) => Expr::NamelessFn(v.to_static()),
            Expr::FnCall(v) => Expr::FnCall(v.to_static()),
            Expr::UnExpr() => Expr::UnExpr(),
            Expr::BinExpr(v) => Expr::BinExpr(v.to_static()),
            Expr::Literal(v) => Expr::Literal(v.to_static()),
            Expr::Path(v) => Expr::Path(v.to_static()),
        }
    }
}
