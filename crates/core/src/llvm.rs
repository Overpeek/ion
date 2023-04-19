use crate::{
    ast,
    ty::{self, FnType, IonType},
};
use inkwell::{
    basic_block::BasicBlock,
    builder::Builder,
    context::Context,
    module::Module,
    support::LLVMString,
    targets::{CodeModel, FileType, RelocMode, Target, TargetMachine},
    types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum},
    values::{BasicValueEnum, FunctionValue},
    AddressSpace, OptimizationLevel,
};
use once_cell::unsync::Lazy;
use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    hash::{Hash, Hasher},
    intrinsics::unreachable,
    path::Path,
};

//

pub struct Compiler<'a> {
    ctx: &'a Context,
    module: Module<'a>,
    builder: Builder<'a>,
    /* lazy_functions: HashMap<&'a str, &'a ast::Fn<'a>>,
    functions: HashMap<String, Function<'a>>, // function name + param types

    current_fn_name: String,
    current_fn: Function<'a>, */
}

#[derive(Debug)]
struct Function<'a> {
    vars: HashMap<&'a str, BasicValueEnum<'a>>,
    ret_ty: Option<Option<BasicTypeEnum<'a>>>,
    // exit: BasicBlock<'a>,
}

impl IonType<'_> {
    fn to_llvm<'a>(&self, ctx: &'a Context) -> BasicTypeEnum<'a> {
        match self {
            IonType::Bool => ctx.bool_type().into(),
            IonType::I32 => ctx.i32_type().into(),
            IonType::F32 => ctx.f32_type().into(),
            IonType::Str => ctx.i8_type().ptr_type(<_>::default()).into(),
            IonType::Fn { name } => ctx.,
            IonType::None => todo!(),
            IonType::Unknown => unreachable!(),
        }
    }

    fn to_params<'a>(&self, ctx: &'a Context) -> BasicMetadataTypeEnum<'a> {
        match self {
            IonType::I32 => ctx.i32_type().into(),
            IonType::F32 => ctx.f32_type().into(),
            IonType::Fn { params, ret } => {
                ret.to_llvm(ctx).ptr_type(AddressSpace::default()).into()
            }
            IonType::None => ctx.struct_type(&[], true).into(),
        }
    }
}

//

impl<'a> Compiler<'a> {
    pub fn compile_ast(ast: &ty::Module, src: Option<&str>) {
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
            /* lazy_functions: <_>::default(),
            functions: <_>::default(),

            current_fn_name: String::new(),
            current_fn: Function {
                vars: <_>::default(),
                ret_ty: None,
            }, */
        }
    }

    fn dump_ir(&self) -> LLVMString {
        self.module.print_to_string()
    }

    fn compile_ast_with(mut self, ast: &ty::Module) {
        // self.compile_mod(ast);

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

    /* fn prepare_mod(&mut self, ast: &ty::Module) {

        self.prepare_fn(&);

        self.compile_fn("_start", &[]);
    }

    fn prepare_fn(&mut self, ast: &ty::Fn) {
        self.add_fn(ast);

        for ast in ast.block.0 .0 {
            self.prepare_stmt(&ast);
        }
    }

    fn prepare_stmt(&mut self, ast: &ast::Stmt) {
        match ast {
            ast::Stmt::Assign(a) => todo!(),
            ast::Stmt::FnCall(_) => todo!(),
            ast::Stmt::Fn(_) => todo!(),
            ast::Stmt::Return(_) => todo!(),
            ast::Stmt::ReturnVoid(_) => todo!(),
            ast::Stmt::Conditional() => todo!(),
            ast::Stmt::IteratorLoop() => todo!(),
            ast::Stmt::Loop() => todo!(),
        }
    }

    fn prepare_assign(&mut self, ast: &ast::Assign) {
        ast.value;
    } */

    fn compile_mod(&self, ast: &ty::Module) {
        for (name, f) in ast.concrete_functions() {
            self.compile_fn(name, f)
        }
    }

    fn compile_fn(&self, name: &str, f: &FnType) {
        self.module.add_function(name, ty, None);
    }

    fn compile_block<'ast>(&self, ast: &'ast ast::Block) -> Option<BasicValueEnum> {
        // let mut ret = None;

        for ast in ast.0 .0.iter() {
            self.compile_stmt(ast);
        }

        // ret
        None
    }

    fn compile_stmt<'ast>(&self, ast: &ast::Stmt) {
        match ast {
            ast::Stmt::Assign(ast) => self.compile_assign(ast),
            ast::Stmt::FnCall(ast) =>
            /* self.compile_fn_call(ast, vars) */
            {
                ()
            }
            // ast::Stmt::Fn(v) => todo!(),
            ast::Stmt::Return(ast) => self.compile_return(ast),
            ast::Stmt::ReturnVoid(ast) => self.compile_return_void(ast),
            _ => todo!(),
            /* ast::Stmt::Conditional() => todo!(),
            ast::Stmt::IteratorLoop() => todo!(),
            ast::Stmt::Loop() => todo!(), */
        }
    }

    fn compile_assign<'ast>(&self, ast: &ast::Assign) {
        let val = self.compile_expr(&ast.value);

        if self.current_fn.vars.insert(&ast.target, val).is_some() {
            todo!("shadow")
        }
    }

    /* fn compile_fn_call(&self, ast: &ast::FnCall, vars: &mut HashMap<&str, BasicValueEnum>) {
        // return value discarded
    } */

    fn compile_return<'ast>(&self, ast: &ast::Return) {
        let value = self.compile_expr(&ast.value);

        if let Some(ret_ty) = self.current_fn.ret_ty {
            assert_eq!(ret_ty, Some(value.get_type()))
        }
        self.current_fn.ret_ty = Some(Some(value.get_type()));

        self.builder.build_return(Some(&value as _));
    }

    fn compile_return_void<'ast>(&self, _: &ast::ReturnVoid) {
        if let Some(ret_ty) = self.current_fn.ret_ty {
            assert_eq!(ret_ty, None)
        }
        self.current_fn.ret_ty = Some(None);

        self.builder.build_return(None);
    }

    fn compile_expr<'ast>(&self, ast: &ast::Expr) -> BasicValueEnum<'a> {
        match ast {
            ast::Expr::NamelessFn(ast) =>
            /* self.compile_fn_def(ast, params) */
            {
                ()
            }
            // ast::Expr::FnCall(_) => todo!(),
            // ast::Expr::UnExpr() => todo!(),
            ast::Expr::BinExpr(ast) => self.compile_bin_expr(ast),
            ast::Expr::Literal(ast) => self.compile_literal(ast),
            ast::Expr::Path(ast) => self.compile_path(ast),
            _ => todo!(),
        }
    }

    fn compile_bin_expr<'ast>(&self, ast: &ast::BinExpr) -> BasicValueEnum<'a> {
        let (lhs, rhs) = (&ast.sides.0, &ast.sides.1);
        let (lhs, rhs) = (self.compile_expr(lhs), self.compile_expr(rhs));

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

    fn compile_path(&self, ast: &ast::Path) -> BasicValueEnum<'a> {
        let path = ast.parts.first().unwrap(); // TODO: structs

        *self
            .current_fn
            .vars
            .get(&path.as_ref())
            .expect("Var not found")
    }
}

//

fn hash(s: impl Hash) -> u64 {
    let mut hasher = DefaultHasher::new();
    s.hash(&mut hasher);
    hasher.finish()
}

fn hash_fn_name(s: &str) -> u64 {
    let mut hasher = DefaultHasher::new();
    s.hash(&mut hasher);
    hasher.finish()
}
