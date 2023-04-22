use inkwell::values::BasicValueEnum;

use crate::ast::Expr;

use super::{Compile, Compiler};

//

impl Compile for Expr<'_> {
    fn compile<'a>(&mut self, compiler: &mut Compiler<'a>) -> Option<BasicValueEnum<'a>> {
        match self {
            Expr::NamelessFn(v) => v.compile(compiler),
            Expr::FnCall(_) => todo!(),
            Expr::UnExpr() => todo!(),
            Expr::BinExpr(v) => v.compile(compiler),
            Expr::Literal(v) => v.compile(compiler),
            Expr::Path(v) => v.compile(compiler),
        }
    }
}
