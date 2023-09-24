use std::fmt::{self, Debug, Display, Formatter, Result};

//

pub struct IterList<I: Iterator>(pub I);

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

impl<'a, I: ExactSizeIterator> Iterator for IterDisplay<'a, I> {
    type Item = IterDisplayPart<'a, I::Item>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(next) = self.next.pop() {
            // println!("return next");
            return Some(next);
        }

        let Some(iter) = self.iter.as_mut() else {
            // println!("return none");
            return None;
        };

        let n = self.n;
        self.n += 1;

        if n == 0 && self.len == 1 {
            // println!("return single");
            return Some(IterDisplayPart::Str(self.single));
        } else if n == 0 && self.len == 0 {
            // println!("return empty");
            return Some(IterDisplayPart::Str(self.empty));
        } else if n == 0 {
            // println!("return multi {}", self.len);
            return Some(IterDisplayPart::Str(self.multiple));
        }

        if n + 2 == self.len * 2 {
            // println!("return lastsep");
            Some(IterDisplayPart::Str(self.last_sep))
        } else if n % 2 == 0 && n != self.len * 2 {
            // println!("return sep");
            Some(IterDisplayPart::Str(self.sep))
        } else {
            // println!("return next?");
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

pub struct Padding(pub u32);

impl Display for Padding {
    fn fmt(&self, f: &mut Formatter) -> Result {
        write!(f, "{:1$}", "", self.0 as usize)
    }
}

//

pub trait PrintSource: Sized {
    fn as_source(&self, indent: u32) -> Source<Self> {
        Source {
            inner: self,
            indent,
        }
    }
}

pub struct Source<'a, T> {
    pub inner: &'a T,
    pub indent: u32,
}

impl<T> PrintSource for T where for<'a> Source<'a, T>: fmt::Display {}
