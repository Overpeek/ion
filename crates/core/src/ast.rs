use crate::DebugTree;
use ion_macros::DebugTree;
use std::{
    borrow::Cow,
    fmt,
    sync::atomic::{AtomicUsize, Ordering},
};

//

/// A name for an identifier
///
/// example: `test_ident`
pub type Ident<'input> = Cow<'input, str>;

/// Source file contents
#[derive(Debug, Clone, Default, DebugTree)]
pub struct Module<'input> {
    pub statements: Statements<'input>,
}

impl<'i> Module<'i> {
    pub fn from_stmt(stmt: Statement<'i>) -> Self {
        Self::default().with_stmt(stmt)
    }

    pub fn with_stmt(mut self, stmt: Statement<'i>) -> Self {
        self.statements.0.push(stmt);
        self
    }
}

// .. and Block and Vec<Statement>
impl<'input> From<Statements<'input>> for Module<'input> {
    fn from(value: Statements<'input>) -> Self {
        Self { statements: value }
    }
}

/// A group of 0 or more statements
#[derive(Debug, Clone, Default)]
pub struct Statements<'input>(pub Vec<Statement<'input>>);

impl DebugTree for Statements<'_> {
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
pub enum Statement<'input> {
    /// An assignment of an [`Expression`] to a name
    Assignment(Assignment<'input>),

    /// An expression
    ///
    /// [`Expression`]
    Expression(Expression<'input>),

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
    Function(Function<'input>),

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
pub struct Assignment<'input> {
    pub target: Ident<'input>,
    pub value: Expression<'input>,
    pub global: bool,
}

impl<'input> Assignment<'input> {
    pub fn new(target: Ident<'input>, value: Expression<'input>) -> Self {
        Self {
            target,
            value,
            global: false,
        }
    }

    pub fn new_global(target: Ident<'input>, value: Expression<'input>) -> Self {
        Self::new(target, value).with_global(true)
    }

    pub fn with_global(mut self, global: bool) -> Self {
        self.global = global;
        self
    }
}

/// Callable function
#[derive(Debug, Clone, DebugTree)]
pub struct Function<'input> {
    pub name: Ident<'input>,
    pub params: Vec<Ident<'input>>,
    pub block: Block<'input>,
}

impl<'input> Function<'input> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with_name(mut self, name: Ident<'input>) -> Self {
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

    pub fn with_params(mut self, params: Vec<Ident<'input>>) -> Self {
        self.params = params;
        self
    }

    pub fn with_block(mut self, block: Block<'input>) -> Self {
        self.block = block;
        self
    }
}

impl<'input> Default for Function<'input> {
    fn default() -> Self {
        Self {
            name: Ident::Borrowed("incomplete function"),
            params: Default::default(),
            block: Default::default(),
        }
    }
}

/// A group of statements surrounded by `{` and `}`
#[derive(Debug, Clone, Default)]
pub struct Block<'input>(pub Statements<'input>);

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
pub enum Expression<'input> {
    /// # A nameless function (or lambda or closure)
    ///
    /// ```text
    /// function(params) {}
    /// ```
    NamelessFunction(Function<'input>),

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
    UnaryOperation(),

    /// # A binary (infix) operation
    ///
    /// ```text
    /// a + b
    /// ```
    /*///
    /// ## Functions can be used as operators
    ///
    /// The first parameter gets the left side and the second parameter gets the right side*/
    BinaryOperation(BinaryOperation<'input>),

    /// A function call
    ///
    /// ```text
    /// a(params)
    /// ```
    FunctionCall(FunctionCall<'input>),
}

#[derive(Debug, Clone)]
pub struct BinaryOperation<'input> {
    pub sides: Box<(Expression<'input>, Expression<'input>)>,
    pub op: &'static str,
}

impl<'input> BinaryOperation<'input> {
    pub fn new(left: Expression<'input>, op: &'static str, right: Expression<'input>) -> Self {
        Self {
            sides: Box::new((left, right)),
            op,
        }
    }
}

impl DebugTree for BinaryOperation<'_> {
    fn fmt(&self, f: &mut fmt::Formatter, depth: u8) -> fmt::Result {
        let d = depth as usize * 2;
        writeln!(f, "BinaryOperation:")?;
        write!(f, "  {:d$}: ", "left", d = d)?;
        DebugTree::fmt(&self.sides.0, f, depth + 1)?;
        write!(f, "  {:d$}: ", "right", d = d)?;
        DebugTree::fmt(&self.sides.1, f, depth + 1)?;
        write!(f, "  {:d$}: ", "op", d = d)?;
        DebugTree::fmt(&self.op, f, depth + 1)?;
        Ok(())
    }
}

#[derive(Debug, Clone, DebugTree)]
pub struct FunctionCall<'input> {
    pub name: Ident<'input>,
    pub params: Vec<Ident<'input>>,
}

impl<'input> FunctionCall<'input> {
    pub fn new(name: Ident<'input>, params: Vec<Ident<'input>>) -> Self {
        Self { name, params }
    }
}
