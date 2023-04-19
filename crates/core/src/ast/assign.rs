use serde::Serialize;

use super::{Expr, Ident, ToStatic};
use crate::ty::IonType;

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
#[derive(Debug, Clone, ToStatic, Serialize)]
pub struct Assign<'i> {
    pub ty: IonType,
    pub target: Ident<'i>,
    pub value: Expr<'i>,
    pub global: bool,
}

impl<'i> Assign<'i> {
    pub fn new(target: Ident<'i>, value: Expr<'i>) -> Self {
        Self {
            ty: IonType::Unknown,
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
