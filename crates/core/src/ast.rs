use crate::DebugTree;
use ion_macros::DebugTree;
use lalrpop_util::{lexer::Token, ParseError};
use std::{
    borrow::Cow,
    collections::HashMap,
    fmt,
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

pub trait Interpret<'i> {
    fn eval(self, local: &mut Vec<HashMap<String, IonItem<'i>>>) -> IonItem<'i>;
}

#[derive(Debug, Clone)]
pub enum IonItem<'i> {
    Int(usize),
    String(String),
    Func(Func<'i>),
    None,
}

/// A name for an identifier
///
/// example: `test_ident`
pub type Ident<'i> = Cow<'i, str>;

/// Source file contents
#[derive(Debug, Clone, Default, DebugTree)]
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
}

impl<'i> Interpret<'i> for Module<'i> {
    fn eval(mut self, local: &mut Vec<HashMap<String, IonItem<'i>>>) -> IonItem<'i> {
        for s in self.statements.0.drain(..) {
            Interpret::eval(s, local);
        }
        IonItem::None
    }
}

// .. and Block and Vec<Statement>
impl<'i> From<Stmts<'i>> for Module<'i> {
    fn from(value: Stmts<'i>) -> Self {
        Self { statements: value }
    }
}

/// A group of 0 or more statements
#[derive(Debug, Clone, Default)]
pub struct Stmts<'i>(pub Vec<Stmt<'i>>);

impl DebugTree for Stmts<'_> {
    fn fmt(&self, f: &mut fmt::Formatter, depth: u8) -> fmt::Result {
        let d = depth as usize * 2;
        writeln!(f, "Statements:")?;

        for i in self.0.iter() {
            write!(f, "  {:d$}", "")?;
            DebugTree::fmt(i, f, depth + 1)?;
        }

        Ok(())
    }
}

/// 1 line of code or a part of a line separated by `;`
#[derive(Debug, Clone, DebugTree)]
pub enum Stmt<'i> {
    /// An assignment of an [`Expression`] to a name
    Assign(Assign<'i>),

    /// An expression
    ///
    /// [`Expression`]
    Expr(Expr<'i>),

    /// A named function
    ///
    /// ```text
    /// function a(params) {}
    /// ```
    ///
    /// this is syntax sugar for:
    ///
    /// ```text
    /// a = function(params) {}
    /// ```
    Func(Func<'i>),

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

impl<'i> Interpret<'i> for Stmt<'i> {
    fn eval(self, local: &mut Vec<HashMap<String, IonItem<'i>>>) -> IonItem<'i> {
        match self {
            Stmt::Assign(v) => v.eval(local),
            Stmt::Expr(v) => v.eval(local),
            Stmt::Func(v) => v.eval(local),
            Stmt::Conditional() => IonItem::None,
            Stmt::IteratorLoop() => IonItem::None,
            Stmt::Loop() => IonItem::None,
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
#[derive(Debug, Clone, DebugTree)]
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

impl<'i> Interpret<'i> for Assign<'i> {
    fn eval(self, local: &mut Vec<HashMap<String, IonItem<'i>>>) -> IonItem<'i> {
        let val = self.value.eval(local);
        let t = self.target.to_string();

        if let Some(g) = local.first_mut().and_then(|g| g.get_mut(&t)) {
            *g = val;
        } else {
            local.last_mut().unwrap().insert(t, val);
        };

        IonItem::None
    }
}

/// Callable function
#[derive(Debug, Clone, DebugTree)]
pub struct Func<'i> {
    pub name: Ident<'i>,
    pub params: Vec<Ident<'i>>,
    pub block: Block<'i>,
}

impl<'i> Func<'i> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_name(mut self, name: Ident<'i>) -> Self {
        self.name = name;
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

impl<'i> Default for Func<'i> {
    fn default() -> Self {
        Self {
            name: Ident::Borrowed("incomplete function"),
            params: Default::default(),
            block: Default::default(),
        }
    }
}

impl<'i> Interpret<'i> for Func<'i> {
    fn eval(self, local: &mut Vec<HashMap<String, IonItem<'i>>>) -> IonItem<'i> {
        IonItem::Func(self)
    }
}

/// A group of statements surrounded by `{` and `}`
#[derive(Debug, Clone, Default)]
pub struct Block<'i>(pub Stmts<'i>);

impl DebugTree for Block<'_> {
    fn fmt(&self, f: &mut fmt::Formatter, depth: u8) -> fmt::Result {
        let d = depth as usize * 2;
        writeln!(f, "Block:")?;

        for i in (self.0).0.iter() {
            write!(f, "  {:d$}", "")?;
            DebugTree::fmt(i, f, depth + 1)?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone, DebugTree)]
pub enum Expr<'i> {
    /// # A nameless function (or lambda or closure)
    ///
    /// ```text
    /// function(params) {}
    /// ```
    NamelessFunc(Func<'i>),

    /// A function call
    ///
    /// ```text
    /// a(params)
    /// ```
    FuncCall(FuncCall<'i>),

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

    /// A path to read from
    ///
    /// ´´´text
    /// a.b.c
    /// ´´´
    Path(Path<'i>),
}

impl<'i> Interpret<'i> for Expr<'i> {
    fn eval(self, local: &mut Vec<HashMap<String, IonItem<'i>>>) -> IonItem<'i> {
        match self {
            Expr::NamelessFunc(v) => v.eval(local),
            Expr::FuncCall(v) => v.eval(local),
            Expr::UnExpr() => IonItem::None,
            Expr::BinExpr(v) => v.eval(local),
            Expr::Literal(v) => v.eval(local),
            Expr::Path(v) => v.eval(local),
        }
    }
}

#[derive(Debug, Clone)]
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

impl DebugTree for BinExpr<'_> {
    fn fmt(&self, f: &mut fmt::Formatter, depth: u8) -> fmt::Result {
        let d = depth as usize * 2;
        writeln!(f, "BinExpr:")?;
        write!(f, "  {:d$}left: ", "", d = d)?;
        DebugTree::fmt(&self.sides.0, f, depth + 1)?;
        write!(f, "  {:d$}right: ", "", d = d)?;
        DebugTree::fmt(&self.sides.1, f, depth + 1)?;
        write!(f, "  {:d$}op: ", "", d = d)?;
        DebugTree::fmt(&self.op, f, depth + 1)?;
        Ok(())
    }
}

impl<'i> Interpret<'i> for BinExpr<'i> {
    fn eval(self, local: &mut Vec<HashMap<String, IonItem<'i>>>) -> IonItem<'i> {
        let (left, right) = *self.sides;
        let (left, right) = (left.eval(local), right.eval(local));
        match (left, self.op, right) {
            (IonItem::Int(a), BinOp::Add, IonItem::Int(b)) => IonItem::Int(a + b),
            (IonItem::Int(a), BinOp::Sub, IonItem::Int(b)) => IonItem::Int(a - b),
            (IonItem::Int(a), BinOp::Mul, IonItem::Int(b)) => IonItem::Int(a * b),
            (IonItem::Int(a), BinOp::Div, IonItem::Int(b)) => IonItem::Int(a / b),

            (IonItem::String(a), BinOp::Add, IonItem::Int(b)) => IonItem::String(format!("{a}{b}")),
            (IonItem::Int(a), BinOp::Add, IonItem::String(b)) => IonItem::String(format!("{a}{b}")),
            (IonItem::String(a), BinOp::Add, IonItem::String(b)) => {
                IonItem::String(format!("{a}{b}"))
            }

            (a, op, b) => panic!("Runtime error: '{op:?}' cannot work with '{a:?}' and '{b:?}'"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

impl DebugTree for BinOp {}

#[derive(Debug, Clone, DebugTree)]
pub struct FuncCall<'i> {
    pub name: Ident<'i>,
    pub args: Vec<Expr<'i>>,
}

impl<'i> FuncCall<'i> {
    pub fn new(name: Ident<'i>, args: Vec<Expr<'i>>) -> Self {
        Self { name, args }
    }
}

impl<'i> Interpret<'i> for FuncCall<'i> {
    fn eval(mut self, local: &mut Vec<HashMap<String, IonItem<'i>>>) -> IonItem<'i> {
        match local
            .first_mut()
            .unwrap()
            .remove(self.name.as_ref()) // TODO: get not remove
            .unwrap_or_else(|| {
                local
                    .last_mut()
                    .unwrap()
                    .remove(self.name.as_ref())
                    .expect("Runtime error: no such variable")
            }) {
            IonItem::Func(Func {
                name,
                params,
                mut block,
            }) => {
                local.push(<_>::default());
                let args: Vec<_> = self.args.drain(..).map(|a| a.eval(local)).collect();
                let f = local.last_mut().unwrap();
                for (p, a) in params.into_iter().zip(args.into_iter()) {
                    f.insert(p.to_string(), a);
                }
                for s in (block.0).0.drain(..) {
                    s.eval(local);
                }
                local.pop();
                IonItem::None
            }
            other => panic!("Runtime error: '{other:?}' isn't callable"),
        }
    }
}

#[derive(Debug, Clone, DebugTree)]
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

impl<'i> Interpret<'i> for Literal<'i> {
    fn eval(self, local: &mut Vec<HashMap<String, IonItem<'i>>>) -> IonItem<'i> {
        match self {
            Literal::Int(i) => IonItem::Int(i),
            Literal::Bool(b) => IonItem::Int(b as _),
            Literal::Float(f) => IonItem::Int(f as _),
            Literal::String(s) => IonItem::String(s.to_string()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Path<'i> {
    pub parts: Vec<Ident<'i>>,
}

impl<'i> Path<'i> {
    pub fn new(parts: Vec<Ident<'i>>) -> Self {
        Self { parts }
    }
}

impl DebugTree for Path<'_> {
    fn debug_tree(&self) -> crate::DebugTreeStruct<Self> {
        crate::DebugTreeStruct(self)
    }

    fn fmt(&self, f: &mut fmt::Formatter, depth: u8) -> fmt::Result {
        let d = depth as usize * 2;
        writeln!(f, "Path:")?;
        for i in self.parts.iter() {
            write!(f, "  {:d$}", "")?;
            DebugTree::fmt(i, f, depth + 1)?;
        }

        Ok(())
    }
}

impl<'i> Interpret<'i> for Path<'i> {
    fn eval(self, local: &mut Vec<HashMap<String, IonItem<'i>>>) -> IonItem<'i> {
        let path = self.parts.first().unwrap(); // TODO:

        local
            .first_mut()
            .unwrap()
            .remove(path.as_ref()) // TODO: get not remove
            .unwrap_or_else(|| {
                local
                    .last_mut()
                    .unwrap()
                    .remove(path.as_ref())
                    .expect("Runtime error: no such variable")
            })
    }
}
