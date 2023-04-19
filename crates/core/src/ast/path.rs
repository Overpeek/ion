use super::{Ident, ToStatic};
use serde::Serialize;

//

/// Data accessor
#[derive(Debug, Clone, ToStatic, Serialize)]
pub struct Path<'i> {
    pub parts: Vec<Ident<'i>>,
}

impl<'i> Path<'i> {
    pub fn new(parts: Vec<Ident<'i>>) -> Self {
        Self { parts }
    }
}
