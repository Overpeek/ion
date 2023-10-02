use std::fmt;

use arcstr::Substr;

use crate::util::{Padding, PrintSource, Source};

//

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[repr(C)]
pub enum Type {
    Struct { id: Substr, fields: Vec<Field> },
    Enum { id: Substr, variants: Vec<Field> },
    Str,
    U64,
    I32,
    F32,
    Bool,
    None,
}

impl Type {
    pub const fn as_str(&self) -> &'static str {
        match self {
            Type::Struct { .. } => "<struct>",
            Type::Enum { .. } => "<enum>",
            Type::Str => "str",
            Type::U64 => "u64",
            Type::I32 => "i32",
            Type::F32 => "f32",
            Type::Bool => "bool",
            Type::None => "none",
        }
    }
}

impl fmt::Display for Source<'_, Type> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.inner {
            Type::Struct { id, fields } => {
                let pad = Padding(self.indent + 4);
                write!(f, "struct {id} {{\n")?;

                for Field { id, ty } in fields {
                    let ty = ty.as_source(self.indent + 4);
                    write!(f, "{pad}{id}: {ty},")?;
                }

                write!(f, "}}")
            }
            Type::Enum { id, variants } => {
                let pad = Padding(self.indent + 4);
                write!(f, "enum {id} {{")?;

                for Field { id, ty } in variants {
                    let ty = ty.as_source(self.indent + 4);
                    write!(f, "{pad}{id}({ty}),")?;
                }

                write!(f, "}}")
            }
            Type::Str => write!(f, "str"),
            Type::U64 => write!(f, "u64"),
            Type::I32 => write!(f, "i32"),
            Type::F32 => write!(f, "f32"),
            Type::Bool => write!(f, "bool"),
            Type::None => write!(f, "none"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Field {
    pub id: Substr,
    pub ty: Type,
}
