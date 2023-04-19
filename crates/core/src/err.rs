use crate::util::{IterDisplay, Padding};
use owo_colors::OwoColorize;
use std::{
    fmt::{self, Display},
    ops::Range,
};
use thiserror::Error;

//

#[derive(Debug, Error, Clone)]
pub enum IonParseError {
    #[error("Invalid token at {row}:{col}")]
    InvalidToken {
        /// char index
        location: usize,

        /// line index
        row: usize,

        /// column index
        col: usize,
    },

    #[error("Unexpected EOF at {row}:{col}, expected one of {expected:?}")]
    UnexpectedEOF {
        /// char index
        location: usize,

        /// line index
        row: usize,

        /// column index
        col: usize,

        expected: Vec<String>,
    },

    #[error("Unexpected token at {}:{}", rows.start, cols.start)]
    UnexpectedToken {
        /// from char index to char index
        token: Range<usize>,

        /// from line index to line index
        rows: Range<usize>,

        /// from column index to colum index
        cols: Range<usize>,

        expected: Vec<String>,
    },

    #[error("Extra token")]
    ExtraToken {
        /// from char index to char index
        token: Range<usize>,

        /// from line index to line index
        rows: Range<usize>,

        /// from column index to colum index
        cols: Range<usize>,
    },

    #[error("{msg}")]
    Other { msg: String },
}

#[derive(Debug, Error, Clone, Copy)]
pub enum IonCompileError {}

#[derive(Debug, Error)]
pub enum IonError {
    #[error("Parse error: {0}")]
    Parse(IonParseError),
}

pub type IonResult<T> = Result<T, IonError>;

#[derive(Debug, Clone, Copy)]
pub struct IonPretty<'i, I> {
    inner: I,
    input: &'i str,
    file: &'i str,
    color: bool,
}

//

impl IonError {
    pub fn pretty_print<'i>(
        &'i self,
        color: bool,
        input: &'i str,
        file: &'i str,
    ) -> IonPretty<'_, &IonError> {
        IonPretty {
            inner: self,
            input,
            file,
            color,
        }
    }
}

impl IonParseError {
    pub fn pretty_print<'i>(
        &'i self,
        color: bool,
        input: &'i str,
        file: &'i str,
    ) -> IonPretty<'i, &Self> {
        IonPretty {
            inner: self,
            input,
            file,
            color,
        }
    }
}

impl fmt::Display for IonPretty<'_, &IonError> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let Self {
            inner,
            input,
            file,
            color,
        } = *self;
        match inner {
            IonError::Parse(inner) => fmt::Display::fmt(
                &IonPretty {
                    inner,
                    input,
                    file,
                    color,
                },
                f,
            ),
        }
    }
}

impl fmt::Display for IonPretty<'_, &IonParseError> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Self {
            inner,
            input,
            file,
            color,
        } = *self;

        macro_rules! col {
            ($color:expr; $(let $i:ident: $col:ident = $s:expr;)+) => {
                $(
                let no_color = $s;
                let color = no_color.$col();
                let $i: &dyn Display = if $color { &color as _ } else { &no_color };
                )+
            };
        }

        col! {
            color;
            let error: red = "error";
            let arrow: blue = "-->";
            let bar: blue = "|";
            let hat: red = "^";
        }

        match inner {
            IonParseError::InvalidToken { row, col, .. } => {
                let (line, pad) = line_and_mag(input, *row);
                let col0 = col;
                let (row, col) = (row + 1, col + 1);
                let row_ = row;

                col! {
                    color;
                    let row: blue = row;
                };

                writeln!(f, "{error}: Invalid token")?;
                writeln!(f, "{pad}{arrow} {file}:{row_}:{col}")?;
                writeln!(f, "{pad} {bar}")?;
                writeln!(f, "{row} {bar} {line}")?;
                writeln!(f, "{pad} {bar} {:col0$}{hat}", "")?
            }
            IonParseError::UnexpectedEOF {
                row, col, expected, ..
            } => {
                let (line, pad) = line_and_mag(input, *row);
                let col0 = col;
                let (row, col) = (row + 1, col + 1);
                let row_ = row;

                col! {
                    color;
                    let row: blue = row;
                };

                writeln!(
                    f,
                    "{error}: Unexpected EOF{}",
                    IterDisplay {
                        iter: expected.iter(),
                        empty: "",
                        single: ", expected ",
                        multiple: ", expected one of ",
                        sep: ", ",
                        last_sep: " or ",
                    }
                )?;
                writeln!(f, "{pad}{arrow} {file}:{row_}:{col}")?;
                writeln!(f, "{pad} {bar}")?;
                writeln!(f, "{row} {bar} {line}")?;
                writeln!(f, "{pad} {bar} {:col0$}{hat}", "")?
            }
            IonParseError::UnexpectedToken {
                rows,
                cols,
                expected,
                ..
            } => {
                let (line, pad) = line_and_mag(input, rows.start);
                let col0 = cols.start;
                let (row, col) = (rows.start + 1, cols.start + 1);
                let row_ = row;

                col! {
                    color;
                    let row: blue = row;
                };

                writeln!(
                    f,
                    "{error}: Unexpected token{}",
                    IterDisplay {
                        iter: expected.iter(),
                        empty: "",
                        single: ", expected ",
                        multiple: ", expected one of ",
                        sep: ", ",
                        last_sep: " or ",
                    }
                )?;
                writeln!(f, "{pad}{arrow} {file}:{row_}:{col}")?;
                writeln!(f, "{pad} {bar}")?;
                writeln!(f, "{row} {bar} {line}")?;

                if !rows.is_empty() {
                    let w1 = line.len() - cols.start;
                    let w2 = cols.end;
                    writeln!(f, "{pad} {bar} {:col0$}{:^>w1$}", "", "")?;

                    let (line, pad) = line_and_mag(input, rows.end);
                    let row = rows.end + 1;

                    col! {
                        color;
                        let row: blue = row;
                    };

                    writeln!(f, "{pad} {bar}")?;
                    writeln!(f, "{row} {bar} {line}")?;
                    writeln!(f, "{pad} {bar} {:^>w2$}", "")?;
                } else {
                    let w = cols.end - cols.start;
                    writeln!(f, "{pad} {bar} {:col0$}{:^>w$}", "", "")?;
                }
            }
            IonParseError::ExtraToken { rows, cols, .. } => {
                let (line, pad) = line_and_mag(input, rows.start);
                let col0 = cols.start;
                let (row, col) = (rows.start + 1, cols.start + 1);
                let row_ = row;

                col! {
                    color;
                    let row: blue = row;
                };

                writeln!(f, "{error}: Extra token")?;
                writeln!(f, "{pad}{arrow} {file}:{row_}:{col}")?;
                writeln!(f, "{pad} {bar}")?;
                writeln!(f, "{row} {bar} {line}")?;

                if !rows.is_empty() {
                    let w1 = line.len() - cols.start;
                    let w2 = cols.end;
                    writeln!(f, "{pad} {bar} {:col0$}{:^>w1$}", "", "")?;

                    let (line, pad) = line_and_mag(input, rows.end);
                    let row = rows.end + 1;

                    col! {
                        color;
                        let row: blue = row;
                    };

                    writeln!(f, "{pad} {bar}")?;
                    writeln!(f, "{row} {bar} {line}",)?;
                    writeln!(f, "{pad} {bar} {:^>w2$}", "")?;
                } else {
                    let w = cols.end - cols.start;
                    writeln!(f, "{pad} {bar} {:col0$}{:^>w$}", "", "")?;
                }
            }
            IonParseError::Other { msg } => {
                writeln!(f, "{error}: Other error: {msg}")?;
            }
        }

        Ok(())
    }
}

//

// convert char index to line str, row and column
pub(crate) fn spot_from_location(location: usize, input: &str) -> Option<(&str, usize, usize)> {
    let mut char_counter = 0;
    input.lines().enumerate().find_map(|(row, line)| {
        let col = char_counter..char_counter + line.len() + 1;
        char_counter = col.end;
        (col.contains(&location)).then_some((line, row, location.saturating_sub(col.start)))
    })
}

fn row_to_line(input: &str, row: usize) -> &str {
    input.lines().nth(row).unwrap_or(input)
}

fn line_and_mag(input: &str, row: usize) -> (&str, Padding) {
    let line = row_to_line(input, row);
    let row_mag = if row == 0 { 1 } else { row.ilog10() + 1 };
    let pad = Padding(row_mag);

    (line, pad)
}

fn last_line_boundary_before(location: usize, input: &str) -> usize {
    input
        .chars()
        .enumerate()
        .take(location)
        .filter(|(_, ch)| *ch == '\n')
        .last()
        .map(|(i, _)| i)
        .unwrap_or(0)
}

fn first_line_boundary_after(location: usize, input: &str) -> usize {
    input
        .chars()
        .enumerate()
        .skip(location)
        .find(|(_, ch)| *ch == '\n')
        .map(|(i, _)| i)
        .unwrap_or(input.len())
}
