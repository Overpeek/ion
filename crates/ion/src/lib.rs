use lalrpop_util::{lalrpop_mod, ParseError};

use self::{
    engine::Engine,
    err::{IonError, IonResult},
    ion::ModuleParser,
    syntax::{lexer::Lexer, Module},
};
use crate::{err::IonParseError, util::PrintSource};

//

lalrpop_mod!(pub ion);
pub mod engine;
pub mod err;
pub mod syntax;
// pub mod ty;
mod util;

//

/// Ion parser, interpreter and compiler
pub struct State {
    parser: ModuleParser,
    engine: Engine,
}

//

impl State {
    pub fn new() -> Self {
        Self {
            parser: ModuleParser::new(),
            engine: Engine::new(),
        }
    }

    pub fn add(&self, base_name: &str, ptr: fn(i32)) {
        self.engine.add(base_name, ptr);
    }

    pub fn run(&self, input: &str) -> IonResult<()> {
        let mut ast = self.parse_str_inner(input)?;
        ast.source_file = Some(arcstr::literal!("<src>"));

        let src = ast.as_source(0);

        println!("{src}");

        Ok(())
    }

    /* pub fn parse_str<'i>(&self, input: &'i str) -> IonResult<Module<'i>> {
        let ast = self.parse_str_inner(input)?;
        // let mut typer = <_>::default();
        // ast.type_of(&mut typer)?;
        // println!("{}", self.to_yaml(&typer));
        Ok(ast)
    }

    pub fn compile_str(&self, input: &str) -> IonResult<()> {
        let mut ast = self.parse_str(input)?;
        self.compile_ast(&mut ast)?;
        Ok(())
    }

    pub fn compile_ast(&self, ast: &mut Module) -> IonResult<()> {
        // println!("{ast}");
        let mut typer = <_>::default();
        ast.type_of(&mut typer)?;
        // println!("type check: {:#?}", ty::Module::new(ast));
        Compiler::compile_ast(ast, &typer, None);
        Ok(())
    }

    pub fn to_yaml(&self, ast: &impl Serialize) -> String {
        serde_yaml::to_string(&ast).unwrap()
    } */

    fn parse_str_inner(&self, input: &str) -> IonResult<Module> {
        let mut errors = vec![];

        let err = match self.parser.parse(&mut errors, Lexer::new(input)) {
            Ok(module) => return Ok(module),
            Err(err) => err,
        };

        let err = match err {
            ParseError::InvalidToken { location } => {
                let (_, row, col) = err::spot_from_location(location, input)
                    .expect("Input doesn't contain the error line");

                IonParseError::InvalidToken { location, row, col }
            }
            ParseError::UnrecognizedEof { location, expected } => {
                let (_, row, col) = err::spot_from_location(location, input)
                    .expect("Input doesn't contain the error line");

                IonParseError::UnexpectedEOF {
                    location,
                    row,
                    col,
                    expected,
                }
            }
            ParseError::UnrecognizedToken { token, expected } => {
                let (_, from_row, from_col) = err::spot_from_location(token.0, input)
                    .expect("Input doesn't contain the error line");
                let (_, to_row, to_col) = err::spot_from_location(token.2, input)
                    .expect("Input doesn't contain the error line");

                IonParseError::UnexpectedToken {
                    token: token.0..token.2,
                    rows: from_row..to_row,
                    cols: from_col..to_col,
                    expected,
                }
            }
            ParseError::ExtraToken { token } => {
                let (_, from_row, from_col) = err::spot_from_location(token.0, input)
                    .expect("Input doesn't contain the error line");
                let (_, to_row, to_col) = err::spot_from_location(token.2, input)
                    .expect("Input doesn't contain the error line");

                IonParseError::ExtraToken {
                    token: token.0..token.2,
                    rows: from_row..to_row,
                    cols: from_col..to_col,
                }
            }
            ParseError::User { error } => IonParseError::Other { msg: error },
        };

        Err(IonError::Parse(err))
    }
}

impl Default for State {
    fn default() -> Self {
        Self::new()
    }
}
