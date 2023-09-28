use arcstr::{ArcStr, Substr};
use logos::{Logos, SpannedIter};

use std::fmt;

//

fn lex_int(lex: &logos::Lexer<Token>) -> Option<i64> {
    lex.slice().parse().ok()
}

fn lex_float(lex: &logos::Lexer<Token>) -> Option<f64> {
    lex.slice().parse().ok()
}

fn lex_bool(lex: &logos::Lexer<Token>) -> Option<bool> {
    lex.slice().parse().ok()
}

fn lex_substr(lex: &logos::Lexer<Token>) -> Option<Substr> {
    let src: &ArcStr = &lex.extras;
    Some(src.substr(lex.span()))
}

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(extras = ArcStr)]
#[logos(skip r"[ \t\n\f]+")] // skip whitespace
#[logos(skip r"//[^\n\r]*[\n\r]*")] // skip line comments
#[logos(skip r"/\*[^*]*\*+(?:[^/*][^*]*\*+)*/")] // skip block comments
pub enum Token {
    #[token("fn")]
    KeywordFn,
    #[token("let")]
    KeywordLet,
    #[token("if")]
    KeywordIf,
    #[token("for")]
    KeywordFor,
    #[token("in")]
    KeywordIn,
    #[token("return")]
    KeywordReturn,

    #[token("i32")]
    KeywordInt,
    #[token("f32")]
    KeywordFloat,
    #[token("bool")]
    KeywordBool,
    #[token("str")]
    KeywordStr,
    #[token("none")]
    KeywordNone,

    #[regex(r"[a-zA-Z_][0-9a-zA-Z_]*", lex_substr)]
    Ident(Substr),

    #[regex(r"\d+", lex_int)]
    Int(i64),
    // #[regex(r"(\d+\.\d*)|(\d*\.\d+)", lex_float)]
    #[regex(r"\d+\.\d+", lex_float)]
    Float(f64),
    #[regex(r#"(true)|(false)"#, lex_bool)]
    Bool(bool),
    #[regex(r#""[^"]*""#, lex_substr)]
    Str(Substr),

    #[token("(")]
    Lparen,
    #[token(")")]
    Rparen,
    #[token("{")]
    Lbrace,
    #[token("}")]
    Rbrace,
    #[token("[")]
    Lbracket,
    #[token("]")]
    Rbracket,
    #[token(",")]
    Comma,
    #[token(";")]
    Semi,
    #[token(":")]
    Colon,

    #[token("+")]
    OpAdd,
    #[token("-")]
    OpSub,
    #[token("*")]
    OpMul,
    #[token("/")]
    OpDiv,
    #[token("%")]
    OpMod,
    #[token("<")]
    OpLt,
    #[token("<=")]
    OpLe,
    #[token("==")]
    OpEq,
    #[token(">=")]
    OpGe,
    #[token(">")]
    OpGt,

    #[token("..")]
    RangeOpen,
    #[token("..=")]
    RangeClosed,

    #[token("=")]
    Assign,
    #[token("+=")]
    AssignAdd,
    #[token("-=")]
    AssignSub,
    #[token("*=")]
    AssignMul,
    #[token("/=")]
    AssignDiv,
    #[token("%=")]
    AssignMod,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{self:?}")
    }
}

//

pub type Spanned<Token, Loc, Error> = Result<(Loc, Token, Loc), Error>;

/* pub enum LexicalError {
    InvalidToken,
} */

pub struct Lexer<'input> {
    token_stream: SpannedIter<'input, Token>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            token_stream: Token::lexer_with_extras(input, ArcStr::from(input)).spanned(),
        }
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = Spanned<Token, usize, String>;

    fn next(&mut self) -> Option<Self::Item> {
        let (token, span) = self.token_stream.next()?;

        Some(match token {
            Ok(token) => Ok((span.start, token, span.end)),
            Err(_) => Err(format!("invalid token: `{}`", self.token_stream.slice())),
        })
    }
}
