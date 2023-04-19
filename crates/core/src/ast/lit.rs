use super::{fsize, ToStatic};
use lalrpop_util::{lexer::Token, ParseError};
use serde::Serialize;
use std::borrow::Cow;

//

#[derive(Debug, Clone, Serialize)]
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
}

impl ToStatic for Literal<'_> {
    type Static = Literal<'static>;

    fn to_static(&self) -> Self::Static {
        match self {
            Literal::Bool(v) => Literal::Bool(*v),
            Literal::Int(v) => Literal::Int(*v),
            Literal::Float(v) => Literal::Float(*v),
            Literal::String(v) => Literal::String(v.clone().into_owned().into()),
        }
    }
}
