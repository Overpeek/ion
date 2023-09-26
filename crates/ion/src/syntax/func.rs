use std::fmt;

use arcstr::Substr;

use crate::util::{IterDisplay, Padding, PrintSource, Source};

use super::{Block, Expr};

//

#[derive(Debug, Clone)]
pub struct FnProto {
    pub id: Substr,
    pub params: ParamList,
    pub ty: Type,
}

impl fmt::Display for Source<'_, FnProto> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let FnProto { id, params, ty } = self.inner;

        let pad = Padding(self.indent);

        let params = params.as_source(self.indent);
        let ty = ty.as_source(self.indent);

        write!(f, "{pad}fn {id}({params}): {ty}")
    }
}

#[derive(Debug, Clone)]
pub struct FnDef {
    pub proto: FnProto,
    pub block: Block,
}

impl fmt::Display for Source<'_, FnDef> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let FnDef { proto, block } = self.inner;

        let pad = Padding(self.indent);

        let proto = proto.as_source(self.indent);
        let block = block.as_source(self.indent + 4);

        write!(f, "{proto}{{\n{block}{pad}}}")
    }
}

#[derive(Debug, Clone)]
pub struct ParamList(pub Vec<Param>);

impl fmt::Display for Source<'_, ParamList> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for part in IterDisplay::new(
            self.inner.0.iter().map(|p| p.as_source(self.indent)),
            "",
            "",
            "",
            ", ",
            ", ",
        ) {
            write!(f, "{part}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct Param {
    pub id: Substr,
    pub ty: Type,
}

impl fmt::Display for Source<'_, Param> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Param { id, ty } = self.inner;
        let ty = ty.as_source(self.indent);

        write!(f, "{id}: {ty}")
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    I32,
    F32,
    Bool,
    None,
}

impl Type {
    pub const fn as_str(&self) -> &'static str {
        match self {
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
            Type::I32 => write!(f, "i32"),
            Type::F32 => write!(f, "f32"),
            Type::Bool => write!(f, "bool"),
            Type::None => write!(f, "none"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FnCall {
    // pub id: Expr,
    pub id: Substr,
    pub args: ArgList,
}

impl fmt::Display for Source<'_, FnCall> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let FnCall { id, args } = self.inner;
        let args = args.as_source(self.indent);

        write!(f, "{id}({args})")
    }
}

#[derive(Debug, Clone)]
pub struct ArgList(pub Vec<Expr>);

impl fmt::Display for Source<'_, ArgList> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for part in IterDisplay::new(
            self.inner.0.iter().map(|e| e.as_source(self.indent)),
            "",
            "",
            "",
            ", ",
            ", ",
        ) {
            write!(f, "{part}")?;
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct FnCallExt {
    pub id: Substr,
    pub addr: usize,
    pub args: ArgList,
}

impl fmt::Display for Source<'_, FnCallExt> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let FnCallExt { id, addr, args } = self.inner;
        let args = args.as_source(self.indent);

        write!(f, "__ion_fncallext_{id}_{addr}({args})")
    }
}
