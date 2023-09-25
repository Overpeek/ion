use inkwell::values::BasicValueEnum;

use super::{Compile, Compiler};
use crate::syntax::Stmt;

//

impl Compile for Stmt<'_> {
    fn compile<'a>(&mut self, compiler: &mut Compiler<'a>) -> Option<BasicValueEnum<'a>> {
        match self {
            Stmt::Assign(v) => v.compile(compiler),
            Stmt::Expr(v) => v.compile(compiler),
            // Stmt::FnCall(_) => todo!(),
            // Stmt::Fn(_) => todo!(),
            Stmt::Return(v) => v.compile(compiler),
            Stmt::ReturnVoid(v) => v.compile(compiler),
            Stmt::Conditional() => todo!(),
            Stmt::IteratorLoop() => todo!(),
            Stmt::Loop() => todo!(),
        }
    }
}
