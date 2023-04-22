use inkwell::values::BasicValueEnum;

use crate::ast::{BinExpr, BinOp};

use super::{Compile, Compiler};

//

impl Compile for BinExpr<'_> {
    fn compile<'a>(&mut self, compiler: &mut Compiler<'a>) -> Option<BasicValueEnum<'a>> {
        let lhs = self.sides.0.compile(compiler)?;
        let rhs = self.sides.1.compile(compiler)?;

        match (lhs, rhs) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => {
                let b = &compiler.builder;
                let n = "int bin expr";
                Some(
                    match self.op {
                        BinOp::Add => b.build_int_add(lhs, rhs, n),
                        BinOp::Sub => b.build_int_sub(lhs, rhs, n),
                        BinOp::Mul => b.build_int_mul(lhs, rhs, n),
                        BinOp::Div => b.build_int_signed_div(lhs, rhs, n),
                    }
                    .into(),
                )
            }
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => {
                let b = &compiler.builder;
                let n = "float bin expr";
                Some(
                    match self.op {
                        BinOp::Add => b.build_float_add(lhs, rhs, n),
                        BinOp::Sub => b.build_float_sub(lhs, rhs, n),
                        BinOp::Mul => b.build_float_mul(lhs, rhs, n),
                        BinOp::Div => b.build_float_div(lhs, rhs, n),
                    }
                    .into(),
                )
            }
            _ => None,
        }
    }
}
