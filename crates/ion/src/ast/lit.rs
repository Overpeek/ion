use std::{borrow::Cow, fmt};

use lalrpop_util::{lexer::Token, ParseError};
use serde::Serialize;

use super::{fsize, ToStatic};

//

#[derive(Debug, Clone, ToStatic, Serialize)]
#[serde(untagged)]
pub enum Literal<'i> {
    Bool(bool),
    Int(usize),
    Float(fsize),
    String(Cow<'i, str>),
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
        Self::String(Cow::Borrowed(&lit[1..lit.len() - 1]))
    }

    pub fn code(&self, f: &mut fmt::Formatter, indent: usize) -> fmt::Result {
        match self {
            Literal::Bool(v) => write!(f, "{v}"),
            Literal::Int(v) => write!(f, "{v}"),
            Literal::Float(v) => write!(f, "{v}"),
            Literal::String(v) => write!(f, "{v}"),
        }
    }
}
