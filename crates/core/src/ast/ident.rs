use std::borrow::Cow;

use super::ToStatic;

//

/// A name for an identifier
///
/// example: `test_ident`
pub type Ident<'i> = Cow<'i, str>;

impl ToStatic for Ident<'_> {
    type Static = Ident<'static>;

    fn to_static(&self) -> Self::Static {
        self.clone().into_owned().into()
    }
}
