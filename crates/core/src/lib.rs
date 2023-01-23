use ast::Module;
use lalrpop_util::{lalrpop_mod, ParseError};
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
        let mut errors = vec![];

        self.parser.parse(&mut errors, s).unwrap_or_else(|err| {
            match err {
                ParseError::InvalidToken { location } => {
                    let (line, row, col) = Self::spot_from_location(location, s)
                        .expect("Input doesn't contain the error line");

                    println!(
                        "Invalid token at <input>:{row}:{col} :\n{line}\n{:col$}^",
                        ""
                    );
                }
                ParseError::UnrecognizedEOF { location, expected } => todo!(),
                ParseError::UnrecognizedToken { token, expected } => {
                    let (line1, row1, col1) = Self::spot_from_location(token.0, s)
                        .expect("Input doesn't contain the error line");
                    let (line2, row2, col2) = Self::spot_from_location(token.2, s)
                        .expect("Input doesn't contain the error line");

                    if row1 == row2 {
                        let w = col2 - col1;
                        println!(
                            "Unrecognized token '{}' at <input>:{row1}{col1} :\n{line1}\n{:col1$}{:^>w$}",
                            token.1,
                            "", ""
                        )
                    }
                    else {
                        let w1 = line1.len() - col1;
                        let w2 = col2 + 1;
                        println!(
                            "Unrecognized token at <input>:{row1}{col1} :\n{line1}\n{:col1$}{:^>w1$}",
                            "", ""
                        );
                        println!(
                            "  ...\n{line2}\n{:^>w2$}",
                            ""
                        );
                    }
                }
                ParseError::ExtraToken { token } => todo!(),
                ParseError::User { error } => todo!(),
            }

            panic!();
        })
    }

    fn spot_from_location(location: usize, input: &str) -> Option<(&str, usize, usize)> {
        let mut char_counter = 0;
        input.lines().enumerate().find_map(|(row, line)| {
            let col = char_counter..char_counter + line.len() + 1;
            char_counter += col.end;
            (col.end >= location).then_some((line, row, location - col.start))
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

    #[allow(unused_variables)]
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
impl<T: DebugTree> DebugTree for Vec<T> {
    fn fmt(&self, f: &mut fmt::Formatter, depth: u8) -> fmt::Result {
        let d = depth as usize * 2;
        writeln!(f)?;
        for i in self.iter() {
            write!(f, "  {:d$}", "")?;
            DebugTree::fmt(i, f, depth + 1)?;
        }

        Ok(())
    }
}

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
