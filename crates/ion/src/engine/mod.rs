use std::{cell::RefCell, collections::HashMap, mem};

use arcstr::{ArcStr, Substr};
use inkwell::{values::BasicMetadataValueEnum, AddressSpace};
use once_cell::unsync::Lazy;

use crate::syntax::{
    BinOp, Block, Expr, FnCall, FnDef, FnExt, FnProto, Item, Let, Module, Param, ParamList, Return,
    Stmt, Type, Value,
};

use llvm::BasicValue;

//

pub mod llvm {
    pub use inkwell::{
        builder::Builder,
        context::Context,
        execution_engine::ExecutionEngine,
        module::{Linkage, Module},
        passes::{PassManager, PassManagerBuilder},
        types::{
            AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType,
        },
        values::{BasicValue, BasicValueEnum, FunctionValue},
        OptimizationLevel,
    };
}

//

/* #[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ProtoNameRef<'a> {
    base: &'a str,
    params: &'a [Type],
    ty: &'a Type,
}

impl<'a> ProtoNameRef<'a> {
    pub const fn new(base: &'a str, params: &'a [Type], ty: &'a Type) -> Self {
        Self { base, params, ty }
    }
}

impl fmt::Display for ProtoNameRef<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ProtoNameRef { base, params, ty } = self;

        write!(f, "{base} [")?;

        for param in self.params.iter().map(Type::as_str) {
            write!(f, "{param};")?;
        }

        write!(f, "]->{}", self.ty.as_str())
    }
}

/* impl<'a> Borrow<ProtoNameRef<'a>> for ProtoName {
    fn borrow(&self) -> &ProtoNameRef<'a> {
        todo!()
    }
} */

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ProtoName {
    base: Substr,
    params: Rc<[Type]>,
    ty: Type,
}

impl ProtoName {
    pub const fn new(base: Substr, params: Rc<[Type]>, ty: Type) -> Self {
        Self { base, params, ty }
    }

    pub fn from_fnproto(fnproto: &FnProto) -> Self {
        Self {
            base: fnproto.id.clone(),
            params: fnproto.params.0.iter().map(|p| p.ty.clone()).collect(),
            ty: fnproto.ty.clone(),
        }
    }

    pub fn as_ref(&self) -> ProtoNameRef {
        ProtoNameRef::new(&self.base, &self.params, &self.ty)
    }
}

impl fmt::Display for ProtoName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.as_ref().fmt(f)
    }
} */

pub struct Engine {
    module: llvm::Module<'static>,
    builder: llvm::Builder<'static>,

    fpm: llvm::PassManager<llvm::FunctionValue<'static>>,

    ee: llvm::ExecutionEngine<'static>,

    fns: RefCell<HashMap<Substr, Function>>,
}

#[derive(Debug, Clone, Copy)]
enum Function {
    Jit {
        func: llvm::FunctionValue<'static>,
    },
    Ext {
        ty: llvm::FunctionType<'static>,
        func: usize,
    },
}

impl Engine {
    pub fn new() -> Self {
        let ctx = get_ctx();
        let module = ctx.create_module("<src>");
        let builder = ctx.create_builder();

        let fpmb = llvm::PassManagerBuilder::create();
        fpmb.set_optimization_level(llvm::OptimizationLevel::Aggressive);
        fpmb.set_inliner_with_threshold(1024);

        // let lpm = llvm::PassManager::create(&());
        // let mpm = llvm::PassManager::create(&());
        let fpm = llvm::PassManager::create(&module);

        // fpmb.populate_lto_pass_manager(&lpm, true, true);
        // fpmb.populate_module_pass_manager(&mpm);
        fpmb.populate_function_pass_manager(&fpm);
        fpm.initialize();

        let ee = module
            .create_jit_execution_engine(llvm::OptimizationLevel::Aggressive)
            .unwrap();

        Self {
            module,
            builder,

            fpm,

            ee,

            fns: RefCell::new(HashMap::new()),
        }
    }

    pub fn dump_ir(&self) -> String {
        self.module.print_to_string().to_string()
    }

    pub fn add(&self, base_name: &str, ptr: fn(i32)) {
        self.load_fnext(&FnExt {
            proto: FnProto {
                id: ArcStr::from(base_name).into(),
                params: ParamList(vec![Param {
                    id: ArcStr::new().into(),
                    ty: Type::I32,
                }]),
                ty: Type::None,
            },
            addr: ptr as usize,
        });

        /* self.load_fnproto(); */
    }

    pub fn run(&self, f: &str) {
        {
            let fns = self.fns.borrow();
            let Function::Jit { func } = fns.get(f).expect("unknown function") else {
                panic!("shouldn't call an extern func");
            };

            if !func.verify(true) {
                panic!("invalid function");
            }

            self.fpm.run_on(func);
        }

        let start = unsafe {
            self.ee
                .get_function::<unsafe extern "C" fn() -> ()>(f)
                .unwrap()
        };

        self.module.strip_debug_info();

        unsafe {
            start.call();
        }
    }

    pub fn load_module(&self, module: &Module) {
        for item in module.items.iter() {
            self.load_item(item)
        }
    }

    pub fn load_item(&self, item: &Item) {
        match item {
            Item::FnDef(fndef) => _ = self.load_fndef(fndef),
        }
    }

    pub fn load_fndef(&self, fndef: &FnDef) -> llvm::FunctionValue<'static> {
        let proto = self.load_fndecl(&fndef.proto);

        let mut scope = Scope::new();

        let entry = self
            .module
            .get_context()
            .append_basic_block(proto, "fn-entry");
        self.builder.position_at_end(entry);

        for (arg, param) in proto.get_param_iter().zip(fndef.proto.params.0.iter()) {
            scope.assign(param.id.clone(), arg);
        }

        self.load_block(&fndef.block, &mut scope);

        self.load_return(&Return(None), &mut scope);

        proto
    }

    pub fn load_fndecl(&self, fnproto: &FnProto) -> llvm::FunctionValue<'static> {
        let (name, ty) = self.load_fnproto_ty(fnproto);
        let proto = self.module.add_function(&name, ty, None);

        for (arg, param) in proto.get_param_iter().zip(fnproto.params.0.iter()) {
            arg.set_name(&param.id);
        }

        self.fns
            .borrow_mut()
            .insert(name, Function::Jit { func: proto });

        proto
    }

    pub fn load_fnext(&self, fnext: &FnExt) {
        let (name, ty) = self.load_fnproto_ty(&fnext.proto);
        let func = fnext.addr;

        self.fns
            .borrow_mut()
            .insert(name, Function::Ext { ty, func });
    }

    pub fn load_fnproto_ty(&self, fnproto: &FnProto) -> (Substr, llvm::FunctionType<'static>) {
        let FnProto { id, params, ty } = fnproto;

        // let name = ProtoName::from_fnproto(fnproto);
        // let name_str = format!("{}", ProtoName::from_fnproto(fnproto));
        let name = id.clone();
        // println!("proto: `{name}`");

        let ty = Self::load_fntype(ty.clone(), params.0.iter().map(|p| &p.ty));

        (name, ty)
    }

    pub fn load_block(&self, block: &Block, scope: &mut Scope) {
        for stmt in block.stmts.iter() {
            self.load_stmt(stmt, scope);
        }
    }

    pub fn load_fntype<'a>(
        ty: Type,
        param_types: impl Iterator<Item = &'a Type>,
    ) -> llvm::FunctionType<'static> {
        let param_types: Vec<llvm::BasicMetadataTypeEnum> =
            param_types.map(Self::load_type).collect();

        let ctx = get_ctx();
        match ty {
            Type::I32 => ctx.i32_type().fn_type(&param_types, false),
            Type::F32 => ctx.f32_type().fn_type(&param_types, false),
            Type::None => ctx.void_type().fn_type(&param_types, false),
        }
    }

    pub fn load_type(ty: &Type) -> llvm::BasicMetadataTypeEnum<'static> {
        let ctx = get_ctx();
        match ty {
            Type::I32 => ctx.i32_type().into(),
            Type::F32 => ctx.f32_type().into(),
            Type::None => ctx.struct_type(&[], false).into(),
        }
    }

    pub fn load_stmt(&self, stmt: &Stmt, scope: &mut Scope) {
        match stmt {
            Stmt::Return(ret) => self.load_return(ret, scope),
            Stmt::Let(r#let) => self.load_let(r#let, scope),
            Stmt::FnCall(fncall) => _ = self.load_fncall(fncall, scope),
        }
    }

    pub fn load_return(&self, ret: &Return, scope: &mut Scope) {
        let val = ret.0.as_ref().map(|expr| self.load_expr(expr, scope));
        let val = val.as_ref().map(|val| val as _);

        self.builder.build_return(val);
    }

    pub fn load_let(&self, r#let: &Let, scope: &mut Scope) {
        let val = self.load_expr(&r#let.expr, scope);
        scope.assign(r#let.id.clone(), val);
    }

    pub fn load_fncall(&self, fncall: &FnCall, scope: &mut Scope) -> llvm::BasicValueEnum<'static> {
        let func = *self.fns.borrow().get(&fncall.id).expect("unknown function");

        let args: Vec<BasicMetadataValueEnum> = fncall
            .args
            .0
            .iter()
            .map(|expr| self.load_expr(expr, scope).into())
            .collect();

        let call = match func {
            Function::Jit { func } => self.builder.build_call(func, &args, "fncall-jit"),
            Function::Ext { ty, func } => {
                let int_ty = get_ctx().ptr_sized_int_type(&self.ee.get_target_data(), None);

                let func = self.builder.build_int_to_ptr(
                    int_ty.const_int(func as _, false),
                    int_ty.ptr_type(AddressSpace::default()),
                    "tmp-int-to-ptr",
                );

                self.builder
                    .build_indirect_call(ty, func, &args, "fncall-ext")
            }
        };

        if let Some(val) = call.try_as_basic_value().left() {
            val
        } else {
            get_ctx()
                .struct_type(&[], false)
                .const_zero()
                .as_basic_value_enum()
        }
    }

    pub fn load_expr(&self, expr: &Expr, scope: &mut Scope) -> llvm::BasicValueEnum<'static> {
        match expr {
            Expr::BinExpr { sides, op } => {
                let lhs = self.load_expr(&sides.0, scope);
                let rhs = self.load_expr(&sides.1, scope);

                match (lhs, rhs, op) {
                    (
                        llvm::BasicValueEnum::IntValue(lhs),
                        llvm::BasicValueEnum::IntValue(rhs),
                        BinOp::Add,
                    ) => self
                        .builder
                        .build_int_add(lhs, rhs, "int-add")
                        .as_basic_value_enum(),

                    (lhs, rhs, op) => {
                        panic!("cannot eval `{op:?}` with `{lhs}` and `{rhs}`");
                    }
                }
            }
            Expr::Value(v) => {
                let ctx = get_ctx();
                match v {
                    Value::Int(v) => ctx
                        .i32_type()
                        .const_int(unsafe { mem::transmute_copy::<i64, u64>(v) }, true)
                        .as_basic_value_enum(),
                    Value::Float(v) => ctx.f32_type().const_float(*v).as_basic_value_enum(),
                    Value::Bool(v) => ctx
                        .bool_type()
                        .const_int(*v as u64, false)
                        .as_basic_value_enum(),
                }
            }
            Expr::Variable(v) => scope.get(v),
            Expr::FnCall(fncall) => self.load_fncall(fncall, scope),
        }
    }
}

impl Default for Engine {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Scope {
    vars: Vec<Vec<Var>>,
}

impl Scope {
    pub const fn new() -> Self {
        Self { vars: vec![] }
    }

    pub fn push(&mut self) {
        self.vars.push(vec![]);
    }

    pub fn pop(&mut self) {
        self.vars.pop();
    }

    pub fn assign(&mut self, id: Substr, val: llvm::BasicValueEnum<'static>) {
        let last = self.last_mut();

        last.push(Var { id, val });
    }

    pub fn set(&mut self, id: &str, val: llvm::BasicValueEnum<'static>) {
        self.vars
            .iter_mut()
            .flat_map(|v| v.iter_mut())
            .rev()
            .find(|var| var.id == id)
            .expect("variable not found")
            .val = val;
    }

    pub fn get(&self, id: &str) -> llvm::BasicValueEnum<'static> {
        self.vars
            .iter()
            .flat_map(|v| v.iter())
            .rev()
            .find(|var| var.id == id)
            .expect("variable not found")
            .val
    }

    fn last_mut(&mut self) -> &mut Vec<Var> {
        if self.vars.len() == 0 {
            self.vars.push(vec![]);
        }

        self.vars.last_mut().unwrap()
    }
}

struct Var {
    id: Substr,
    val: llvm::BasicValueEnum<'static>,
}

//

thread_local! {
    static CTX: Lazy<&'static llvm::Context> = Lazy::new(|| {
        Box::leak(Box::new(llvm::Context::create()))
    });
}

fn get_ctx() -> &'static llvm::Context {
    CTX.with(|ctx| *Lazy::force(ctx))
}
