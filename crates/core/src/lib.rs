use ast::Module;
use lalrpop_util::lalrpop_mod;
use std::{borrow::Cow, fmt};

//

lalrpop_mod!(pub grammar);
pub mod ast;

//

/// Ion parser, interpreter and compiler
pub struct Ion {
    parser: grammar::ModuleParser,
}

impl Ion {
    pub fn new() -> Self {
        Self {
            parser: grammar::ModuleParser::new(),
        }
    }

    pub fn parse_str<'input>(&'input self, s: &'input str) -> Module<'input> {
        self.parser.parse(s).unwrap_or_else(|err| {
            println!("Parse error {err:?}");
            panic!();
        })
    }
}

impl Default for Ion {
    fn default() -> Self {
        Self::new()
    }
}

pub trait DebugTree: fmt::Debug {
    fn debug_tree(&self) -> DebugTreeStruct<Self> {
        DebugTreeStruct(self)
    }

    fn fmt(&self, f: &mut fmt::Formatter, depth: u8) -> fmt::Result {
        writeln!(f, "{self:?}")
    }
}

impl DebugTree for bool {}
impl DebugTree for u8 {}
impl DebugTree for u16 {}
impl DebugTree for u32 {}
impl DebugTree for u64 {}
impl DebugTree for usize {}
impl DebugTree for i8 {}
impl DebugTree for i16 {}
impl DebugTree for i32 {}
impl DebugTree for i64 {}
impl DebugTree for isize {}
impl DebugTree for f32 {}
impl DebugTree for f64 {}
impl DebugTree for str {}
impl DebugTree for &str {}
impl<T> DebugTree for Cow<'_, T>
where
    T: DebugTree + ToOwned + ?Sized,
    <T as ToOwned>::Owned: fmt::Debug,
{
}
impl<T: DebugTree> DebugTree for Vec<T> {}

pub struct DebugTreeStruct<'a, T: DebugTree + ?Sized>(&'a T);

impl<T: DebugTree> fmt::Display for DebugTreeStruct<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        DebugTree::fmt(self.0, f, 0)
    }
}

impl<T: DebugTree> fmt::Debug for DebugTreeStruct<'_, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        DebugTree::fmt(self.0, f, 0)
    }
}
