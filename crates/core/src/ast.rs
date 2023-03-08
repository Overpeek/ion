use lalrpop_util::{lexer::Token, ParseError};
use serde::Serialize;
use std::{
    borrow::Cow,
    sync::atomic::{AtomicUsize, Ordering},
};

//

#[cfg(target_pointer_width = "64")]
#[allow(non_camel_case_types)]
type fsize = f64;
#[cfg(not(target_pointer_width = "64"))]
#[allow(non_camel_case_types)]
type fsize = f32;

//

/// A name for an identifier
///
/// example: `test_ident`
pub type Ident<'i> = Cow<'i, str>;

/// Source file contents
#[derive(Debug, Clone, Default, Serialize)]
#[serde(transparent)]
pub struct Module<'i> {
    pub statements: Stmts<'i>,
}

impl<'i> Module<'i> {
    pub fn from_stmt(stmt: Stmt<'i>) -> Self {
        Self::default().with_stmt(stmt)
    }

    pub fn with_stmt(mut self, stmt: Stmt<'i>) -> Self {
        self.statements.0.push(stmt);
        self
    }

    /* fn to_static(&self) -> Self {
        self.statements.0.iter().map(|stmt| stmt.to_static()).
    } */
}

// .. and Block and Vec<Statement>
impl<'i> From<Stmts<'i>> for Module<'i> {
    fn from(value: Stmts<'i>) -> Self {
        Self { statements: value }
    }
}

/// A group of 0 or more statements
#[derive(Debug, Clone, Default, Serialize)]
pub struct Stmts<'i>(pub Vec<Stmt<'i>>);

/// 1 line of code or a part of a line separated by `;`
#[derive(Debug, Clone, Serialize)]
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

/// An assignment of an [`Expression`] to a name
///
/// ```text
/// a = value
/// ```
///
/// Global assignment:
///
/// ```text
/// global a = value
/// ```
///
#[derive(Debug, Clone, Serialize)]
pub struct Assign<'i> {
    pub target: Ident<'i>,
    pub value: Expr<'i>,
    pub global: bool,
}

impl<'i> Assign<'i> {
    pub fn new(target: Ident<'i>, value: Expr<'i>) -> Self {
        Self {
            target,
            value,
            global: false,
        }
    }

    pub fn new_global(target: Ident<'i>, value: Expr<'i>) -> Self {
        Self::new(target, value).with_global(true)
    }

    pub fn with_global(mut self, global: bool) -> Self {
        self.global = global;
        self
    }
}

/// Callable function
///
/// TODO: extern functions (bodyless)
#[derive(Debug, Clone, Serialize)]
pub struct Fn<'i> {
    pub name: Ident<'i>,
    pub params: Vec<Ident<'i>>,
    pub block: Block<'i>,
}

impl<'i> Fn<'i> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_name(mut self, name: impl Into<Ident<'i>>) -> Self {
        self.name = name.into();
        self
    }

    pub fn with_random_name(mut self) -> Self {
        // xorshift
        //
        // not super stable with multiple threads
        static ID: AtomicUsize = AtomicUsize::new(0x5555555555555555);
        ID.fetch_xor(ID.load(Ordering::SeqCst) << 13, Ordering::SeqCst);
        ID.fetch_xor(ID.load(Ordering::SeqCst) >> 7, Ordering::SeqCst);
        let id = ID.fetch_xor(ID.load(Ordering::SeqCst) << 17, Ordering::SeqCst);

        // TODO: more context
        self.name = Cow::Owned(format!("nameless function {id}"));
        self
    }

    pub fn with_params(mut self, params: Vec<Ident<'i>>) -> Self {
        self.params = params;
        self
    }

    pub fn with_block(mut self, block: Block<'i>) -> Self {
        self.block = block;
        self
    }
}

impl<'i> Default for Fn<'i> {
    fn default() -> Self {
        Self {
            name: Ident::Borrowed("incomplete function"),
            params: Default::default(),
            block: Default::default(),
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct Return<'i> {
    pub value: Expr<'i>,
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct ReturnVoid;

/// A group of statements surrounded by `{` and `}`
#[derive(Debug, Clone, Default, Serialize)]
pub struct Block<'i>(pub Stmts<'i>);

#[derive(Debug, Clone, Serialize)]
pub enum Expr<'i> {
    /// # A nameless function (or lambda or closure)
    ///
    /// ```text
    /// function(params) {}
    /// ```
    NamelessFn(Fn<'i>),

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

#[derive(Debug, Clone, Serialize)]
pub struct BinExpr<'i> {
    pub sides: Box<(Expr<'i>, Expr<'i>)>,
    pub op: BinOp,
}

impl<'i> BinExpr<'i> {
    pub fn new(left: Expr<'i>, op: BinOp, right: Expr<'i>) -> Self {
        Self {
            sides: Box::new((left, right)),
            op,
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, Serialize)]
pub struct FnCall<'i> {
    pub name: Ident<'i>,
    pub args: Vec<Expr<'i>>,
}

impl<'i> FnCall<'i> {
    pub fn new(name: Ident<'i>, args: Vec<Expr<'i>>) -> Self {
        Self { name, args }
    }
}

#[derive(Debug, Clone, Copy, Serialize)]
#[serde(untagged)]
pub enum Literal<'i> {
    Int(usize),
    Bool(bool),
    Float(fsize),
    String(&'i str),
}

impl<'i> Literal<'i> {
    pub fn parse_int(lit: &str) -> Result<Self, ParseError<usize, Token, String>> {
        lit.parse().map(Self::Int).map_err(|err| ParseError::User {
            error: format!("Invalid int literal: {err}"),
        })
    }

    pub fn parse_float(lit: &str) -> Result<Self, ParseError<usize, Token, String>> {
        lit.parse()
            .map(Self::Float)
            .map_err(|err| ParseError::User {
                error: format!("Invalid float literal: {err}"),
            })
    }

    pub fn parse_str(lit: &'i str) -> Self {
        Self::String(&lit[1..lit.len() - 1])
    }
}

/// Data accessor
#[derive(Debug, Clone, Serialize)]
pub struct Path<'i> {
    pub parts: Vec<Ident<'i>>,
}

impl<'i> Path<'i> {
    pub fn new(parts: Vec<Ident<'i>>) -> Self {
        Self { parts }
    }
}
