use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module as LlvmModule,
    support::LLVMString,
    targets::{CodeModel, FileType, RelocMode, Target, TargetMachine},
    types::{AnyTypeEnum, BasicMetadataTypeEnum, FunctionType},
    values::BasicValueEnum,
    OptimizationLevel,
};
use once_cell::unsync::Lazy;

use crate::{prelude::Module, ty::IonType};

//

mod assign;
mod binexpr;
mod expr;
mod func;
mod lit;
mod path;
mod stmt;

//

pub struct Compiler<'a> {
    ctx: &'a Context,
    module: LlvmModule<'a>,
    builder: Builder<'a>,

    vars: HashMap<String, Vec<BasicValueEnum<'a>>>,
}

impl<'a> Compiler<'a> {
    pub fn compile_ast(ast: &mut Module, src: Option<&str>) {
        thread_local! {
            static CTX: Lazy<Context> = Lazy::new(Context::create);
        }

        CTX.with(|ctx| {
            Compiler::new(ctx, src).compile_ast_with(ast); // Self would borrow for
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

            vars: <_>::default(),
        }
    }

    fn dump_ir(&self) -> LLVMString {
        self.module.print_to_string()
    }

    fn compile_ast_with(mut self, ast: &mut Module) {
        ast.compile(&mut self);

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

        Target::initialize_all(&<_>::default());

        let triple = TargetMachine::get_default_triple();
        println!("{}", triple);
        let target = Target::from_triple(&triple).unwrap();

        // triple cpu
        // TODO: random cpus like specific ARM CPUs
        let cpu = "generic"; // "x86-64"

        // enable all native features `-march=native`
        let features = TargetMachine::get_host_cpu_features();
        let features = features.to_str().unwrap();

        let target = target
            .create_target_machine(
                &triple,
                cpu,
                features,
                OptimizationLevel::Default,
                RelocMode::Default,
                CodeModel::Default,
            )
            .unwrap();

        target
            .write_to_file(
                &self.module,
                FileType::Object,
                std::path::Path::new("out.o"),
            )
            .unwrap();
    }
}

impl IonType {
    fn ll_type<'a>(&self, ctx: &'a Context) -> AnyTypeEnum<'a> {
        match self {
            IonType::Bool => ctx.bool_type().into(),
            IonType::Int => ctx.i32_type().into(),
            IonType::Float => ctx.f32_type().into(),
            IonType::Str => ctx.i8_type().ptr_type(<_>::default()).into(),
            IonType::Void => ctx.void_type().into(),

            // TODO: captures
            IonType::NamelessFn { .. } => ctx.struct_type(&[], false).into(),

            IonType::Struct => ctx.struct_type(&[], false).into(),
            IonType::Tuple => ctx.struct_type(&[], false).into(),

            IonType::Unknown => unreachable!(),
        }
    }

    fn ll_fn_type<'a>(
        &self,
        ctx: &'a Context,
        param_types: &[BasicMetadataTypeEnum<'a>],
        is_var_args: bool,
    ) -> FunctionType<'a> {
        match self {
            IonType::Bool => ctx.bool_type().fn_type(param_types, is_var_args),
            IonType::Int => ctx.i32_type().fn_type(param_types, is_var_args),
            IonType::Float => ctx.f32_type().fn_type(param_types, is_var_args),
            IonType::Str => ctx
                .i8_type()
                .ptr_type(<_>::default())
                .fn_type(param_types, is_var_args),
            IonType::Void => ctx.void_type().fn_type(param_types, is_var_args),

            // TODO: captures
            IonType::NamelessFn { .. } => ctx
                .struct_type(&[], false)
                .fn_type(param_types, is_var_args),

            IonType::Struct => ctx
                .struct_type(&[], false)
                .fn_type(param_types, is_var_args),
            IonType::Tuple => ctx
                .struct_type(&[], false)
                .fn_type(param_types, is_var_args),

            IonType::Unknown => unreachable!(),
        }
    }
}

pub trait Compile {
    fn compile<'a>(&mut self, compiler: &mut Compiler<'a>) -> Option<BasicValueEnum<'a>>;
}

impl Compile for Module<'_> {
    fn compile<'a>(&mut self, compiler: &mut Compiler<'a>) -> Option<BasicValueEnum<'a>> {
        self.start.compile(compiler)
    }
}

/* impl Compile for Block<'_> {
    fn compile<'a>(&mut self, compiler: & mut Compiler<'a>) -> BasicTypeEnum {
        for stmt in self.stmts.iter_mut() {
            stmt.compile(compiler);
        }
    }
} */
