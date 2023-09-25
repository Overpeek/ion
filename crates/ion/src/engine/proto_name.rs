//

/* #[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ProtoNameRef<'a> {
    base: &'a str,
    params: &'a [Type],
    ty: &'a Type,
}

impl<'a> ProtoNameRef<'a> {
    pub const fn new(base: &'a str, params: &'a [Type], ty: &'a Type) -> Self {
        Self { base, params, ty }
    }
}

impl fmt::Display for ProtoNameRef<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ProtoNameRef { base, params, ty } = self;

        write!(f, "{base} [")?;

        for param in self.params.iter().map(Type::as_str) {
            write!(f, "{param};")?;
        }

        write!(f, "]->{}", self.ty.as_str())
    }
}

/* impl<'a> Borrow<ProtoNameRef<'a>> for ProtoName {
    fn borrow(&self) -> &ProtoNameRef<'a> {
        todo!()
    }
} */

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ProtoName {
    base: Substr,
    params: Rc<[Type]>,
    ty: Type,
}

impl ProtoName {
    pub const fn new(base: Substr, params: Rc<[Type]>, ty: Type) -> Self {
        Self { base, params, ty }
    }

    pub fn from_fnproto(fnproto: &FnProto) -> Self {
        Self {
            base: fnproto.id.clone(),
            params: fnproto.params.0.iter().map(|p| p.ty.clone()).collect(),
            ty: fnproto.ty.clone(),
        }
    }

    pub fn as_ref(&self) -> ProtoNameRef {
        ProtoNameRef::new(&self.base, &self.params, &self.ty)
    }
}

impl fmt::Display for ProtoName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.as_ref().fmt(f)
    }
} */
