use crate::ast;
use inkwell::{
    builder::Builder, context::Context, module::Module, support::LLVMString,
    types::BasicMetadataTypeEnum, values::FunctionValue,
};
use once_cell::unsync::Lazy;

//

pub struct Compiler<'a> {
    ctx: &'a Context,
    module: Module<'a>,
    builder: Builder<'a>,
}

//

impl<'a> Compiler<'a> {
    pub fn compile_ast(ast: &ast::Module, src: Option<&str>) {
        thread_local! {
            static CTX: Lazy<Context> = Lazy::new(Context::create);
        }

        CTX.with(|ctx| {
            Compiler::new(ctx, src).compile_ast_with(ast); // Self would borrow for 'a
        });
    }

    fn new(ctx: &'a Context, src: Option<&str>) -> Self {
        let module = ctx.create_module("module");
        let builder = ctx.create_builder();

        if let Some(src) = src {
            module.set_source_file_name(src);
        }

        Self {
            ctx,
            module,
            builder,
        }
    }

    fn dump_ir(&self) -> LLVMString {
        self.module.print_to_string()
    }

    fn compile_ast_with(self, ast: &ast::Module) {
        self.compile_mod(ast);

        if let Ok(ir) = self.dump_ir().to_str() {
            println!(
                r#"
=====================
 BEGIN LLVM-ir DUMP:
=====================
{ir}
=====================
 END LLVM-ir DUMP
====================="#
            );
        } else {
            println!("IR contains invalid UTF-8")
        }
    }

    fn compile_mod(&self, ast: &ast::Module) {
        let main_fn = ast::Func::new()
            .with_name("main")
            .with_params(vec![])
            .with_block(ast::Block(ast.statements.clone()));

        self.compile_fn_dec(&main_fn, &[]);
    }

    fn compile_fn_dec(
        &self,
        ast: &ast::Func,
        params: &'a [BasicMetadataTypeEnum],
    ) -> FunctionValue {
        debug_assert_eq!(ast.params.len(), params.len());

        let fn_typ = self.ctx.i32_type().fn_type(params, false);
        let fn_val = self.module.add_function(&ast.name, fn_typ, None);

        for (param, name) in fn_val.get_param_iter().zip(ast.params.iter()) {
            param.set_name(name);
        }

        fn_val
    }

    fn compile_fn_def(
        &self,
        ast: &ast::Func,
        params: &'a [BasicMetadataTypeEnum],
    ) -> FunctionValue {
        let fn_val = self.compile_fn_dec(ast, params);

        fn_val
    }
}
