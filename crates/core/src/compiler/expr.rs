use inkwell::values::BasicValueEnum;

use super::{Compile, Compiler};
use crate::ast::Expr;

//

impl Compile for Expr<'_> {
    fn compile<'a>(&mut self, compiler: &mut Compiler<'a>) -> Option<BasicValueEnum<'a>> {
        match self {
            Expr::NamelessFn(v) => v.compile(compiler),
            Expr::FnCall(v) => v.compile(compiler),
            Expr::UnExpr() => todo!(),
            Expr::BinExpr(v) => v.compile(compiler),
            Expr::Literal(v) => v.compile(compiler),
            Expr::Path(v) => v.compile(compiler),
        }
    }
}
