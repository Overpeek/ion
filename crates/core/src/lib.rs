use lalrpop_util::{lalrpop_mod, ParseError};
use serde::Serialize;

use self::{
    ast::Module,
    compiler::Compiler,
    err::{IonError, IonResult},
    ty::ResolveType,
};
use crate::err::IonParseError;

//

lalrpop_mod!(pub grammar);
pub mod ast;
pub mod compiler;
pub mod err;
pub mod prelude;
pub mod ty;
mod util;

//

/// Ion parser, interpreter and compiler
pub struct Ion {
    parser: grammar::ModuleParser,
}

//

impl Ion {
    pub fn new() -> Self {
        Self {
            parser: grammar::ModuleParser::new(),
        }
    }

    pub fn parse_str<'i>(&self, input: &'i str) -> IonResult<Module<'i>> {
        let mut ast = self.parse_str_inner(input)?;
        let mut typer = <_>::default();
        ast.type_of(&mut typer)?;
        // println!("{}", self.to_yaml(&typer));
        Ok(ast)
    }

    pub fn compile_str(&self, input: &str) -> IonResult<()> {
        let mut ast = self.parse_str(input)?;
        self.compile_ast(&mut ast)?;
        Ok(())
    }

    pub fn compile_ast(&self, ast: &mut Module) -> IonResult<()> {
        // println!("type check: {:#?}", ty::Module::new(ast));
        Compiler::compile_ast(ast, None);
        Ok(())
    }

    pub fn to_yaml(&self, ast: &impl Serialize) -> String {
        serde_yaml::to_string(&ast).unwrap()
    }

    fn parse_str_inner<'i>(&self, input: &'i str) -> IonResult<Module<'i>> {
        let mut errors = vec![];

        let res = self.parser.parse(&mut errors, input);

        let err = match res {
            Ok(module) => return Ok(module),
            Err(err) => err,
        };

        let err = match err {
            ParseError::InvalidToken { location } => {
                let (_, row, col) = err::spot_from_location(location, input)
                    .expect("Input doesn't contain the error line");

                IonParseError::InvalidToken { location, row, col }
            }
            ParseError::UnrecognizedEOF { location, expected } => {
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

impl Default for Ion {
    fn default() -> Self {
        Self::new()
    }
}

//

#[cfg(test)]
mod tests {
    use std::process::exit;

    use crate::Ion;

    #[test]
    fn invalid_token() {
        let code = r#"x = \"#;
        let ion = Ion::new();
        let err = ion.parse_str(code).unwrap_err();

        insta::assert_display_snapshot!(err.pretty_print(false, code, "<code>"));
    }

    #[test]
    fn unexpected_eof() {
        let code = r#"x ="#;
        let ion = Ion::new();
        let err = ion.parse_str(code).unwrap_err();

        insta::assert_display_snapshot!(err.pretty_print(false, code, "<code>"));
    }

    #[test]
    fn unexpected_token_1() {
        let code = r#"x = ="#;
        let ion = Ion::new();
        let err = ion.parse_str(code).unwrap_err();

        insta::assert_display_snapshot!(err.pretty_print(false, code, "<code>"));
    }

    #[test]
    fn unexpected_token_2() {
        let code = r#""


            ""#;
        let ion = Ion::new();
        let err = ion.parse_str(code).unwrap_err();

        insta::assert_display_snapshot!(err.pretty_print(false, code, "<code>"));
    }

    #[test]
    fn correct_code() {
        let code = r#"
            x = 4;
            fn y() { x = 2; };
            y();"#;

        let ion = Ion::new();
        ion.parse_str(code).unwrap_or_else(|err| {
            eprintln!("{}", err.pretty_print(true, code, "<code>"));
            exit(-1);
        });
    }
}
