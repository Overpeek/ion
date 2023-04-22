use std::{
    collections::HashMap,
    fmt::{Debug, Display, Formatter, Result},
    hash::Hash,
    iter::Peekable,
};

use crate::ast::fsize;

//

pub struct IterList<I: Iterator>(pub I);

pub struct Padding(pub u32);

#[derive(Clone)]
pub struct IterDisplay<'s, I: ExactSizeIterator> {
    pub iter: Option<I>,
    pub empty: &'s str,
    pub single: &'s str,
    pub multiple: &'s str,
    pub sep: &'s str,
    pub last_sep: &'s str,
    len: usize,
    n: usize,
    next: Vec<IterDisplayPart<'s, I::Item>>,
}

#[derive(Clone)]
pub enum IterDisplayPart<'s, I> {
    Item(I),
    Str(&'s str),
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

impl<'a, I: ExactSizeIterator> IterDisplay<'a, I> {
    pub fn new(
        iter: I,
        empty: &'a str,
        single: &'a str,
        multiple: &'a str,
        sep: &'a str,
        last_sep: &'a str,
    ) -> Self {
        let len = iter.len();
        let iter = Some(iter);
        Self {
            iter,
            empty,
            single,
            multiple,
            sep,
            last_sep,
            len,
            n: 0,
            next: vec![],
        }
    }
}

impl<'a, I: Display> Display for IterDisplayPart<'a, I> {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match self {
            IterDisplayPart::Item(item) => write!(f, "{item}"),
            IterDisplayPart::Str(s) => write!(f, "{s}"),
        }
    }
}

impl<'a, I: ExactSizeIterator> Iterator for IterDisplay<'a, I>
where
    I::Item: Display,
    I: Clone,
{
    type Item = IterDisplayPart<'a, I::Item>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.next.pop() {
            return Some(next);
        }

        let Some(iter) = self.iter.as_mut() else {
            return None;
        };

        let n = self.n;
        self.n += 1;

        if n == 0 && self.len == 0 {
            return Some(IterDisplayPart::Str(self.single));
        } else if n == 0 && self.len == 1 {
            return Some(IterDisplayPart::Str(self.empty));
        } else if n == 0 {
            return Some(IterDisplayPart::Str(self.multiple));
        }

        if n + 2 == self.len * 2 {
            Some(IterDisplayPart::Str(self.last_sep))
        } else if n % 2 == 0 && n != self.len * 2 {
            Some(IterDisplayPart::Str(self.sep))
        } else {
            Some(IterDisplayPart::Item(iter.next()?))
        }
    }
}

/* impl<I: ExactSizeIterator> Display for IterDisplay<'_, I>
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
} */

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

impl<T: ToStatic> ToStatic for Option<T> {
    type Static = Option<T::Static>;

    fn to_static(&self) -> Self::Static {
        self.as_ref().map(|v| v.to_static())
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
