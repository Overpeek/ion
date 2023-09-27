use std::{
    cell::RefCell,
    sync::atomic::{AtomicUsize, Ordering},
};

use lalrpop_util::{lalrpop_mod, ParseError};

use self::{
    engine::Engine,
    err::{IonError, IonResult},
    ion::{ChunkParser, ModuleParser},
    syntax::{
        lexer::{Lexer, Token},
        FnDef, FnProto, Module,
    },
};
use crate::{
    err::IonParseError,
    syntax::{Item, ParamList, Type},
    util::PrintSource,
};

//

lalrpop_mod!(pub ion);
pub mod engine;
pub mod err;
pub mod syntax;
// pub mod ty;
mod util;

//

/// Ion parser, interpreter and compiler
pub struct State {
    module_parser: ModuleParser,
    chunk_parser: ChunkParser,

    engine: Engine,

    src: RefCell<Module>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum OptLevel {
    High,
    #[default]
    Medium,
    Low,
    None,
}

#[derive(Debug, Clone, Copy)]
#[repr(C, u8)]
pub enum RuntimeValue {
    U64(u64) = 4,
    I32(i32) = 3,
    F32(f32) = 2,
    Bool(bool) = 1,
    None = 0,
}

pub trait AsIonType: Sized {
    const TYPE: Type;

    fn to_runtime(self) -> RuntimeValue;

    fn from_runtime(runtime_val: RuntimeValue) -> Option<Self>;
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

pub trait IonCallback<F> {
    const ARGS: &'static [Type];
    const TYPE: Type;

    fn args(&self) -> &'static [Type] {
        Self::ARGS
    }

    fn ty(&self) -> Type {
        Self::TYPE.clone()
    }

    fn call(&mut self, args: &[RuntimeValue]) -> RuntimeValue;
}

impl<F, R> IonCallback<(R,)> for F
where
    F: FnMut() -> R,
    R: AsIonType,
{
    const ARGS: &'static [Type] = &[];
    const TYPE: Type = R::TYPE;

    fn call(&mut self, args: &[RuntimeValue]) -> RuntimeValue {
        assert_eq!(args.len(), 0);
        AsIonType::to_runtime((self)())
    }
}

impl<F, A1, R> IonCallback<(A1, R)> for F
where
    F: FnMut(A1) -> R,
    A1: AsIonType,
    R: AsIonType,
{
    const ARGS: &'static [Type] = &[A1::TYPE];
    const TYPE: Type = R::TYPE;

    fn call(&mut self, args: &[RuntimeValue]) -> RuntimeValue {
        assert_eq!(args.len(), 1);

        let a1 = A1::from_runtime(args[0]).expect("runtime type error");

        AsIonType::to_runtime((self)(a1))
    }
}

impl<F, A1, A2, R> IonCallback<(A1, A2, R)> for F
where
    F: FnMut(A1, A2) -> R,
    A1: AsIonType,
    A2: AsIonType,
    R: AsIonType,
{
    const ARGS: &'static [Type] = &[A1::TYPE, A2::TYPE];
    const TYPE: Type = R::TYPE;

    fn call(&mut self, args: &[RuntimeValue]) -> RuntimeValue {
        assert_eq!(args.len(), 2);

        let a1 = A1::from_runtime(args[0]).expect("runtime type error");
        let a2 = A2::from_runtime(args[1]).expect("runtime type error");

        AsIonType::to_runtime((self)(a1, a2))
    }
}

//

impl State {
    pub fn new() -> Self {
        Self {
            module_parser: ModuleParser::new(),
            chunk_parser: ChunkParser::new(),
            engine: Engine::default(),

            src: RefCell::new(Module {
                src_files: vec![],
                items: vec![],
            }),
        }
    }

    pub fn with_opt_level(mut self, opt_level: OptLevel) -> Self {
        self.set_opt_level(opt_level);
        self
    }

    pub fn set_opt_level(&mut self, opt_level: OptLevel) {
        self.engine.set_opt_level(opt_level);
    }

    pub fn add<C, F>(&self, base_name: &str, func: C)
    where
        C: IonCallback<F> + 'static,
    {
        self.engine.add(base_name, func);
    }

    // pub unsafe fn add_ext(&self, name: &str) {}

    pub fn include_module(&self, input: &str) -> IonResult<()> {
        let mut errors = vec![];

        let mut module = self
            .module_parser
            .parse(&mut errors, Lexer::new(input))
            .map_err(|err| Self::map_err(input, err, &errors))?;

        module.src_files.push(arcstr::literal!("<src>"));

        self.engine.load_module(&module);
        self.src.borrow_mut().extend(module);

        Ok(())
    }

    pub fn run(&self, input: &str) -> IonResult<()> {
        let mut errors = vec![];

        let chunk = self
            .chunk_parser
            .parse(&mut errors, Lexer::new(input))
            .map_err(|err| Self::map_err(input, err, &errors))?;

        static N: AtomicUsize = AtomicUsize::new(0);
        let id = arcstr::format!("__ion_immediate_run_{}", N.fetch_add(1, Ordering::Relaxed));

        let fndef = FnDef {
            proto: FnProto {
                id: id.substr(..),
                params: ParamList(vec![]),
                ty: Type::None,
            },
            block: chunk,
        };

        self.engine.load_fndef(&fndef);
        self.src.borrow_mut().items.push(Item::FnDef(fndef));

        self.engine.run(&id);

        Ok(())
    }

    /* pub fn run(&self, f: &str) {
        self.engine.run(f);
    } */

    pub fn dump_src(&self) -> String {
        let src = self.src.borrow();
        let src = src.as_source(0);
        format!("{src}")
    }

    pub fn dump_ir(&self) -> String {
        self.engine.dump_ir()
    }

    /* pub fn parse_str<'i>(&self, input: &'i str) -> IonResult<Module<'i>> {
        let ast = self.parse_str_inner(input)?;
        // let mut typer = <_>::default();
        // ast.type_of(&mut typer)?;
        // println!("{}", self.to_yaml(&typer));
        Ok(ast)
    }

    pub fn compile_str(&self, input: &str) -> IonResult<()> {
        let mut ast = self.parse_str(input)?;
        self.compile_ast(&mut ast)?;
        Ok(())
    }

    pub fn compile_ast(&self, ast: &mut Module) -> IonResult<()> {
        // println!("{ast}");
        let mut typer = <_>::default();
        ast.type_of(&mut typer)?;
        // println!("type check: {:#?}", ty::Module::new(ast));
        Compiler::compile_ast(ast, &typer, None);
        Ok(())
    }

    pub fn to_yaml(&self, ast: &impl Serialize) -> String {
        serde_yaml::to_string(&ast).unwrap()
    } */

    fn map_err(input: &str, err: ParseError<usize, Token, String>, _errors: &[String]) -> IonError {
        let err = match err {
            ParseError::InvalidToken { location } => {
                let (_, row, col) = err::spot_from_location(location, input)
                    .expect("Input doesn't contain the error line");

                IonParseError::InvalidToken { location, row, col }
            }
            ParseError::UnrecognizedEof { location, expected } => {
                let (_, row, col) = err::spot_from_location(location, input)
                    .expect("Input doesn't contain the error line");

                IonParseError::UnexpectedEOF {
                    location,
                    row,
                    col,
                    expected,
                }
            }
            ParseError::UnrecognizedToken { token, expected } => {
                let (_, from_row, from_col) = err::spot_from_location(token.0, input)
                    .expect("Input doesn't contain the error line");
                let (_, to_row, to_col) = err::spot_from_location(token.2, input)
                    .expect("Input doesn't contain the error line");

                IonParseError::UnexpectedToken {
                    token: token.0..token.2,
                    rows: from_row..to_row,
                    cols: from_col..to_col,
                    expected,
                }
            }
            ParseError::ExtraToken { token } => {
                let (_, from_row, from_col) = err::spot_from_location(token.0, input)
                    .expect("Input doesn't contain the error line");
                let (_, to_row, to_col) = err::spot_from_location(token.2, input)
                    .expect("Input doesn't contain the error line");

                IonParseError::ExtraToken {
                    token: token.0..token.2,
                    rows: from_row..to_row,
                    cols: from_col..to_col,
                }
            }
            ParseError::User { error } => IonParseError::Other { msg: error },
        };

        IonError::Parse(err)
    }
}

impl Default for State {
    fn default() -> Self {
        Self::new()
    }
}
