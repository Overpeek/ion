use std::slice::from_raw_parts;

use crate::ty::Type;

//

#[derive(Debug, Clone, Copy)]
#[repr(C, u8)]
pub enum RuntimeValue {
    Str(u64, u64) = 5,
    U64(u64) = 4,
    I32(i32) = 3,
    F32(f32) = 2,
    Bool(bool) = 1,
    None = 0,
}

impl RuntimeValue {
    pub const fn size_of() -> usize {
        std::mem::size_of::<Self>()
    }

    pub const fn align_of() -> usize {
        std::mem::align_of::<Self>()
    }

    pub const fn type_of(&self) -> Type {
        match self {
            RuntimeValue::Str(_, _) => Type::Str,
            RuntimeValue::U64(_) => Type::U64,
            RuntimeValue::I32(_) => Type::I32,
            RuntimeValue::F32(_) => Type::F32,
            RuntimeValue::Bool(_) => Type::Bool,
            RuntimeValue::None => Type::None,
        }
    }

    pub const fn tag(ty: &Type) -> u8 {
        match ty {
            Type::Str => 5,
            Type::U64 => 4,
            Type::I32 => 3,
            Type::F32 => 2,
            Type::Bool => 1,
            Type::None => 0,
            _ => todo!(),
        }
    }
}

pub trait AsIonType: Sized {
    const TYPE: Type;

    fn to_runtime(self) -> RuntimeValue;

    fn from_runtime(runtime_val: RuntimeValue) -> Option<Self>;

    fn from_runtime_unwrap(runtime_val: RuntimeValue) -> Self {
        Self::from_runtime(runtime_val).unwrap_or_else(|| {
            let exp = Self::TYPE.as_str();
            let got = runtime_val.type_of().as_str();
            panic!("runtime type error, expected `{exp}`, got `{got}`");
        })
    }
}

impl AsIonType for &str {
    const TYPE: Type = Type::Str;

    fn to_runtime(self) -> RuntimeValue {
        RuntimeValue::Str(self.len() as u64, self.as_ptr() as u64)
    }

    fn from_runtime(runtime_val: RuntimeValue) -> Option<Self> {
        match runtime_val {
            RuntimeValue::Str(len, ptr) => {
                let bytes = unsafe { from_raw_parts(ptr as *const u8, len as usize) };
                Some(std::str::from_utf8(bytes).expect("ion strings should always be utf8"))
            }
            _ => None,
        }
    }
}

impl AsIonType for u64 {
    const TYPE: Type = Type::U64;

    fn to_runtime(self) -> RuntimeValue {
        RuntimeValue::U64(self)
    }

    fn from_runtime(runtime_val: RuntimeValue) -> Option<Self> {
        match runtime_val {
            RuntimeValue::U64(v) => Some(v),
            _ => None,
        }
    }
}

impl AsIonType for i32 {
    const TYPE: Type = Type::I32;

    fn to_runtime(self) -> RuntimeValue {
        RuntimeValue::I32(self)
    }

    fn from_runtime(runtime_val: RuntimeValue) -> Option<Self> {
        match runtime_val {
            RuntimeValue::I32(v) => Some(v),
            _ => None,
        }
    }
}

impl AsIonType for f32 {
    const TYPE: Type = Type::F32;

    fn to_runtime(self) -> RuntimeValue {
        RuntimeValue::F32(self)
    }

    fn from_runtime(runtime_val: RuntimeValue) -> Option<Self> {
        match runtime_val {
            RuntimeValue::F32(v) => Some(v),
            _ => None,
        }
    }
}

impl AsIonType for bool {
    const TYPE: Type = Type::Bool;

    fn to_runtime(self) -> RuntimeValue {
        RuntimeValue::Bool(self)
    }

    fn from_runtime(runtime_val: RuntimeValue) -> Option<Self> {
        match runtime_val {
            RuntimeValue::Bool(v) => Some(v),
            _ => None,
        }
    }
}

impl AsIonType for () {
    const TYPE: Type = Type::None;

    fn to_runtime(self) -> RuntimeValue {
        RuntimeValue::None
    }

    fn from_runtime(runtime_val: RuntimeValue) -> Option<Self> {
        match runtime_val {
            RuntimeValue::None => Some(()),
            _ => None,
        }
    }
}
