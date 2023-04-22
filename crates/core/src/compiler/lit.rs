use inkwell::values::BasicValueEnum;

use super::{Compile, Compiler};
use crate::ast::Literal;

//

impl Compile for Literal<'_> {
    fn compile<'a>(&mut self, compiler: &mut Compiler<'a>) -> Option<BasicValueEnum<'a>> {
        Some(match self {
            Literal::Int(i) => compiler.ctx.i32_type().const_int(*i as _, false).into(),
            Literal::Bool(b) => compiler.ctx.bool_type().const_int(*b as _, false).into(),
            Literal::Float(f) => compiler.ctx.f32_type().const_float(*f as _).into(),
            Literal::String(s) => compiler.ctx.const_string(s.as_bytes(), false).into(),
        })
    }
}
