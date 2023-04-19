use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module as LlvmModule,
    support::LLVMString,
    targets::{CodeModel, FileType, RelocMode, Target, TargetMachine},
    types::{AnyTypeEnum, BasicMetadataTypeEnum, BasicTypeEnum, FunctionType},
    values::BasicValueEnum,
    OptimizationLevel,
};
use once_cell::unsync::Lazy;

use crate::{
    ast::{Assign, BinExpr, BinOp, Expr, Fn, Literal, Path, Return, Stmt},
    prelude::Module,
    ty::{IonType, ResolveType},
};

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

//

impl Compile for Module<'_> {
    fn compile<'a>(&mut self, compiler: &mut Compiler<'a>) -> Option<BasicValueEnum<'a>> {
        self.start.compile(compiler)
    }
}

impl Compile for Fn<'_> {
    fn compile<'a>(&mut self, compiler: &mut Compiler<'a>) -> Option<BasicValueEnum<'a>> {
        // TODO:

        let ty = self.type_of_assume_resolved();

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

/* impl Compile for Block<'_> {
    fn compile<'a>(&mut self, compiler: & mut Compiler<'a>) -> BasicTypeEnum {
        for stmt in self.stmts.iter_mut() {
            stmt.compile(compiler);
        }
    }
} */

impl Compile for Stmt<'_> {
    fn compile<'a>(&mut self, compiler: &mut Compiler<'a>) -> Option<BasicValueEnum<'a>> {
        match self {
            Stmt::Assign(v) => v.compile(compiler),
            Stmt::FnCall(_) => todo!(),
            Stmt::Fn(_) => todo!(),
            Stmt::Return(v) => v.compile(compiler),
            Stmt::ReturnVoid(_) => todo!(),
            Stmt::Conditional() => todo!(),
            Stmt::IteratorLoop() => todo!(),
            Stmt::Loop() => todo!(),
        }
    }
}

impl Compile for Assign<'_> {
    fn compile<'a>(&mut self, compiler: &mut Compiler<'a>) -> Option<BasicValueEnum<'a>> {
        let value = self.value.compile(compiler).expect("Expr returned nothing");
        let key = self.target.to_string();

        compiler.vars.entry(key).or_default().push(value);

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

impl Compile for Expr<'_> {
    fn compile<'a>(&mut self, compiler: &mut Compiler<'a>) -> Option<BasicValueEnum<'a>> {
        match self {
            Expr::NamelessFn(_) => todo!(),
            Expr::FnCall(_) => todo!(),
            Expr::UnExpr() => todo!(),
            Expr::BinExpr(v) => v.compile(compiler),
            Expr::Literal(v) => v.compile(compiler),
            Expr::Path(v) => v.compile(compiler),
        }
    }
}

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

impl Compile for Path<'_> {
    fn compile<'a>(&mut self, compiler: &mut Compiler<'a>) -> Option<BasicValueEnum<'a>> {
        let part = self.parts.first().unwrap(); // TODO: whole path
        let value = *compiler.vars.get(part.as_ref()).unwrap().last().unwrap();
        Some(value)
    }
}
