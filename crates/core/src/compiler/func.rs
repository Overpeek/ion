use inkwell::values::{BasicValueEnum, FunctionValue};

use super::{Compile, Compiler};
use crate::{
    ast::{Fn, FnCall, Return, ReturnVoid},
    ty::{IonType, ResolveType},
};

//

impl Fn<'_> {
    pub fn compile_proto<'a>(&mut self, compiler: &mut Compiler<'a>) -> FunctionValue<'a> {
        let ty = self.block.type_of_resolved().unwrap();

        println!("{} == {ty:?}", self.name);

        let params: Vec<_> = self
            .params
            .iter()
            .map(|param| param.0.ll_metadata_type(compiler.ctx))
            .collect();

        let f = ty.ll_fn_type(compiler.ctx, &params[..], false);
        if self.name == "_start" {
            compiler.module.add_function(self.name.as_ref(), f, None)
        } else {
            compiler.module.add_function(
                &format!("{} nfn {}", self.name.as_ref(), rand::random::<usize>()),
                f,
                None,
            )
        }
    }

    pub fn compile_body<'a>(
        &mut self,
        compiler: &mut Compiler<'a>,
        proto: FunctionValue<'a>,
    ) -> Option<()> {
        // TODO:

        let entry = compiler.ctx.append_basic_block(proto, "entry");
        compiler.builder.position_at_end(entry);

        for stmt in self.block.stmts.iter_mut() {
            stmt.compile(compiler);
        }

        Some(())
    }
}

impl Compile for Fn<'_> {
    fn compile<'a>(&mut self, compiler: &mut Compiler<'a>) -> Option<BasicValueEnum<'a>> {
        Some(
            compiler
                .ctx
                .struct_type(&[], false)
                .const_named_struct(&[])
                .into(),
        )
    }
}

impl Compile for Return<'_> {
    fn compile<'a>(&mut self, compiler: &mut Compiler<'a>) -> Option<BasicValueEnum<'a>> {
        let value = self.value.compile(compiler)?;
        println!("building return for {value}");
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

impl Compile for FnCall<'_> {
    fn compile<'a>(&mut self, compiler: &mut Compiler<'a>) -> Option<BasicValueEnum<'a>> {
        /* let var = compiler
        .vars
        .get(self.name.as_ref())
        .and_then(|var| var.last())
        .unwrap(); */

        let fn_id = match self.fn_ty {
            IonType::NamelessFn { id } => id,
            _ => unreachable!(),
        };

        let args = self
            .args
            .iter_mut()
            .map(|arg| Some(arg.compile(compiler)?.try_into().unwrap()))
            .collect::<Option<Vec<_>>>()?;

        let params: Vec<IonType> = self
            .args
            .iter()
            .map(|arg| arg.type_of_resolved().unwrap())
            .collect();

        let f = *compiler.fns.get(&fn_id).unwrap().get(&params[..]).unwrap();

        Some(
            compiler
                .builder
                .build_call(f, &args[..], "fn-call")
                .try_as_basic_value()
                .left()
                .unwrap_or_else(|| compiler.ctx.struct_type(&[], false).const_zero().into()),
        )
    }
}
