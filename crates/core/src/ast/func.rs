use super::{Block, Expr, Ident, ToStatic};
use serde::Serialize;
use std::borrow::Cow;

//

/// Callable function
///
/// TODO: extern functions (bodyless)
#[derive(Debug, Clone, ToStatic, Serialize)]
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
        // TODO: more context
        let id: usize = rand::random();
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

impl Default for Fn<'_> {
    fn default() -> Self {
        Self {
            name: Ident::Borrowed("incomplete function"),
            params: Default::default(),
            block: Default::default(),
        }
    }
}

#[derive(Debug, Clone, ToStatic, Serialize)]
pub struct FnCall<'i> {
    pub name: Ident<'i>,
    pub args: Vec<Expr<'i>>,
}

impl<'i> FnCall<'i> {
    pub fn new(name: Ident<'i>, args: Vec<Expr<'i>>) -> Self {
        Self { name, args }
    }
}

#[derive(Debug, Clone, ToStatic, Serialize)]
pub struct Return<'i> {
    pub value: Expr<'i>,
}

#[derive(Debug, Clone, ToStatic, Default, Serialize)]
#[to_static(result = "Self")]
pub struct ReturnVoid;
