use inkwell::values::BasicValueEnum;

use super::{Compile, Compiler};
use crate::{
    ast::{Fn, Return, ReturnVoid},
    ty::ResolveType,
};

//

impl Compile for Fn<'_> {
    fn compile<'a>(&mut self, compiler: &mut Compiler<'a>) -> Option<BasicValueEnum<'a>> {
        // TODO:

        let ty = self.type_of_resolved().unwrap();

        let f = ty.ll_fn_type(compiler.ctx, &[], false);
        let f = compiler.module.add_function("_start", f, None);

        let entry = compiler.ctx.append_basic_block(f, "entry");
        compiler.builder.position_at_end(entry);

        for stmt in self.block.stmts.iter_mut() {
            stmt.compile(compiler);
        }

        None
    }
}

impl Compile for Return<'_> {
    fn compile<'a>(&mut self, compiler: &mut Compiler<'a>) -> Option<BasicValueEnum<'a>> {
        let value = self.value.compile(compiler)?;
        compiler.builder.build_return(Some(&value));
        None
    }
}

impl Compile for ReturnVoid {
    fn compile<'a>(&mut self, compiler: &mut Compiler<'a>) -> Option<BasicValueEnum<'a>> {
        compiler.builder.build_return(None);
        None
    }
}
