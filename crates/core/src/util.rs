use std::{
    collections::HashMap,
    fmt::{Debug, Display, Formatter, Result},
    hash::Hash,
    sync::atomic::AtomicU32,
};

use crate::ast::fsize;

//

pub struct IterList<I: Iterator>(pub I);

pub struct Padding(pub u32);

#[derive(Clone, Copy)]
pub struct IterDisplay<'s, I: ExactSizeIterator>
where
    I::Item: Display,
{
    pub iter: I,
    pub empty: &'s str,
    pub single: &'s str,
    pub multiple: &'s str,
    pub sep: &'s str,
    pub last_sep: &'s str,
}

//

pub trait MakeIterList: Iterator + Sized {
    fn debug_iter(self) -> IterList<Self>;
}

//

impl<I: Iterator> MakeIterList for I {
    fn debug_iter(self) -> IterList<Self> {
        IterList(self)
    }
}

impl<I: Iterator> Debug for IterList<I>
where
    I::Item: Debug,
    I: Clone,
{
    fn fmt(&self, f: &mut Formatter) -> Result {
        f.debug_list().entries(self.0.clone()).finish()
    }
}

impl Display for Padding {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{:1$}", "", self.0 as usize)
    }
}

impl<I: ExactSizeIterator> Display for IterDisplay<'_, I>
where
    I::Item: Display,
    I: Clone,
{
    fn fmt(&self, f: &mut Formatter) -> Result {
        let mut iter = self.iter.clone();
        match iter.len() {
            0 => write!(f, "{}", self.empty)?,
            1 => write!(f, "{}{}", self.single, iter.next().unwrap())?,
            _ => {
                let first = iter.next().unwrap();
                write!(f, "{}{first}", self.multiple)?;

                while let Some(next) = iter.next() {
                    if iter.len() == 0 {
                        write!(f, "{}", self.last_sep)?
                    } else {
                        write!(f, "{}", self.sep)?
                    };
                    write!(f, "{next}")?;
                }
            }
        }

        Ok(())
    }
}

//

pub trait ToStatic {
    type Static;

    fn to_static(&self) -> Self::Static;
}

impl<K: ToStatic, V: ToStatic> ToStatic for HashMap<K, V>
where
    K::Static: Hash + Eq,
{
    type Static = HashMap<K::Static, V::Static>;

    fn to_static(&self) -> Self::Static {
        self.iter()
            .map(|(k, v)| (k.to_static(), v.to_static()))
            .collect()
    }
}

impl<T: ToStatic> ToStatic for Vec<T> {
    type Static = Vec<T::Static>;

    fn to_static(&self) -> Self::Static {
        self.iter().map(|item| item.to_static()).collect()
    }
}

impl<T: ToStatic> ToStatic for Box<T> {
    type Static = Box<T::Static>;

    fn to_static(&self) -> Self::Static {
        Box::new(self.as_ref().to_static())
    }
}

impl ToStatic for String {
    type Static = String;

    fn to_static(&self) -> Self::Static {
        self.clone()
    }
}

impl<A: ToStatic, B: ToStatic> ToStatic for (A, B) {
    type Static = (A::Static, B::Static);

    fn to_static(&self) -> Self::Static {
        (self.0.to_static(), self.1.to_static())
    }
}

impl ToStatic for u32 {
    type Static = Self;

    fn to_static(&self) -> Self::Static {
        *self
    }
}

impl ToStatic for usize {
    type Static = Self;

    fn to_static(&self) -> Self::Static {
        *self
    }
}

impl ToStatic for fsize {
    type Static = Self;

    fn to_static(&self) -> Self::Static {
        *self
    }
}

impl ToStatic for bool {
    type Static = Self;

    fn to_static(&self) -> Self::Static {
        *self
    }
}
