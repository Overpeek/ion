use crate::ast;
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    support::LLVMString,
    targets::{CodeModel, FileType, RelocMode, Target, TargetMachine},
    types::{BasicMetadataTypeEnum, BasicTypeEnum},
    values::{BasicValueEnum, FunctionValue},
    OptimizationLevel,
};
use once_cell::unsync::Lazy;
use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    hash::{Hash, Hasher},
    path::Path,
};

//

pub struct Compiler<'a> {
    ctx: &'a Context,
    module: Module<'a>,
    builder: Builder<'a>,
}

#[derive(Debug)]
struct Function<'a> {
    vars: HashMap<u64, BasicValueEnum<'a>>,
    ret_ty: Option<Option<BasicTypeEnum<'a>>>,
    // exit: BasicBlock<'a>,
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
            .write_to_file(&self.module, FileType::Object, Path::new("out.o"))
            .unwrap();
    }

    fn compile_mod(&self, ast: &ast::Module) {
        let main_fn = ast::Fn::new()
            .with_name("_start")
            .with_params(vec![])
            .with_block(ast::Block(ast.statements.clone()));

        self.compile_fn_def(&main_fn, &[]);
    }

    fn compile_fn_dec(
        &self,
        ast: &ast::Fn,
        params: &'a [BasicMetadataTypeEnum],
    ) -> FunctionValue<'a> {
        debug_assert_eq!(ast.params.len(), params.len());

        let fn_typ = self.ctx.i32_type().fn_type(params, false);
        let fn_val = self.module.add_function(&ast.name, fn_typ, None);

        for (param, name) in fn_val.get_param_iter().zip(ast.params.iter()) {
            param.set_name(name);
        }

        fn_val
    }

    fn compile_fn_def(&self, ast: &ast::Fn, params: &'a [BasicMetadataTypeEnum]) {
        let fn_val = self.compile_fn_dec(ast, params);
        let entry = self.ctx.append_basic_block(fn_val, "entry");
        // let exit = self.ctx.append_basic_block(fn_val, "exit");

        self.builder.position_at_end(entry);

        let mut vars = HashMap::new();
        for (param, name) in fn_val.get_param_iter().zip(ast.params.iter()) {
            vars.insert(hash_fn_name(name), param);
            /* let alloca = self.builder.build_alloca(param.get_type(), name);

            self.builder.build_store(alloca, param); */
        }
        let mut f = Function {
            vars,
            ret_ty: None,
            // exit,
        };

        let ret = self.compile_block(&ast.block, &mut f);

        // self.builder.build_unconditional_branch(exit);
        // self.builder.position_at_end(exit);
        // self.builder.build_return(ret.as_ref().map(|v| v as _));
    }

    fn compile_block(&self, ast: &ast::Block, f: &mut Function<'a>) -> Option<BasicValueEnum> {
        // let mut ret = None;

        for ast in ast.0 .0.iter() {
            self.compile_stmt(ast, f);
        }

        // ret
        None
    }

    fn compile_stmt(&self, ast: &ast::Stmt, f: &mut Function<'a>) {
        match ast {
            ast::Stmt::Assign(ast) => self.compile_assign(ast, f),
            ast::Stmt::FnCall(ast) =>
            /* self.compile_fn_call(ast, vars) */
            {
                ()
            }
            // ast::Stmt::Fn(v) => todo!(),
            ast::Stmt::Return(ast) => self.compile_return(ast, f),
            ast::Stmt::ReturnVoid(ast) => self.compile_return_void(ast, f),
            _ => todo!(),
            /* ast::Stmt::Conditional() => todo!(),
            ast::Stmt::IteratorLoop() => todo!(),
            ast::Stmt::Loop() => todo!(), */
        }
    }

    fn compile_assign(&self, ast: &ast::Assign, f: &mut Function<'a>) {
        let val = self.compile_expr(&ast.value, f);

        if f.vars.insert(hash_fn_name(&ast.target), val).is_some() {
            // shadow
        }
    }

    /* fn compile_fn_call(&self, ast: &ast::FnCall, vars: &mut HashMap<&str, BasicValueEnum>) {
        // return value discarded
    } */

    fn compile_return(&self, ast: &ast::Return, f: &mut Function<'a>) {
        let value = self.compile_expr(&ast.value, f);

        if let Some(ret_ty) = f.ret_ty {
            assert_eq!(ret_ty, Some(value.get_type()))
        }
        f.ret_ty = Some(Some(value.get_type()));

        self.builder.build_return(Some(&value as _));
    }

    fn compile_return_void(&self, _: &ast::ReturnVoid, f: &mut Function<'a>) {
        if let Some(ret_ty) = f.ret_ty {
            assert_eq!(ret_ty, None)
        }
        f.ret_ty = Some(None);

        self.builder.build_return(None);
    }

    fn compile_expr(&self, ast: &ast::Expr, f: &mut Function<'a>) -> BasicValueEnum<'a> {
        match ast {
            ast::Expr::NamelessFn(ast) => todo!(),
            // ast::Expr::FnCall(_) => todo!(),
            // ast::Expr::UnExpr() => todo!(),
            ast::Expr::BinExpr(ast) => self.compile_bin_expr(ast, f),
            ast::Expr::Literal(ast) => self.compile_literal(ast),
            ast::Expr::Path(ast) => self.compile_path(ast, f),
            _ => todo!(),
        }
    }

    fn compile_bin_expr(&self, ast: &ast::BinExpr, f: &mut Function<'a>) -> BasicValueEnum<'a> {
        let (lhs, rhs) = (&ast.sides.0, &ast.sides.1);
        let (lhs, rhs) = (self.compile_expr(lhs, f), self.compile_expr(rhs, f));

        match (lhs, rhs) {
            (BasicValueEnum::IntValue(lhs), BasicValueEnum::IntValue(rhs)) => self
                .builder
                .build_int_add(lhs, rhs, "BinExpr int add")
                .into(),
            (BasicValueEnum::FloatValue(lhs), BasicValueEnum::FloatValue(rhs)) => self
                .builder
                .build_float_add(lhs, rhs, "BinExpr float add")
                .into(),
            _ => todo!(),
        }
    }

    fn compile_literal(&self, ast: &ast::Literal) -> BasicValueEnum<'a> {
        match *ast {
            ast::Literal::Int(i) => self.ctx.i32_type().const_int(i as _, false).into(),
            ast::Literal::Bool(b) => self.ctx.bool_type().const_int(b as _, false).into(),
            ast::Literal::Float(f) => self.ctx.f32_type().const_float(f as _).into(),
            ast::Literal::String(s) => self.ctx.const_string(s.as_bytes(), false).into(),
        }
    }

    fn compile_path(&self, ast: &ast::Path, f: &mut Function<'a>) -> BasicValueEnum<'a> {
        let path = ast.parts.first().unwrap(); // TODO: structs

        *f.vars.get(&hash_fn_name(path)).expect("Var not found")
    }
}

//

fn hash_fn_name(s: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    s.hash(&mut hasher);
    hasher.finish()
}
