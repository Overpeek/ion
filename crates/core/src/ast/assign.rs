use super::{Expr, Ident, ToStatic};
use serde::Serialize;

//

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
#[derive(Debug, Clone, Serialize)]
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

impl ToStatic for Assign<'_> {
    type Static = Assign<'static>;

    fn to_static(&self) -> Self::Static {
        let target: std::borrow::Cow<'static, str> = self.target.to_static();
        Self::Static {
            target,
            value: self.value.to_static(),
            global: self.global,
        }
    }
}
