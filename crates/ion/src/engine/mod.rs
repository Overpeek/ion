use std::{cell::RefCell, collections::HashMap, mem};

use arcstr::{ArcStr, Substr};
use once_cell::unsync::Lazy;

use crate::syntax::{
    ArgList, Assign, AssignOp, BinOp, Block, Expr, FnCall, FnCallExt, FnDef, FnProto, Item, Let,
    Module, Param, ParamList, Return, Stmt, Type, Value,
};

use llvm::BasicValue;

//

pub mod llvm {
    pub use inkwell::{
        basic_block::BasicBlock,
        builder::Builder,
        context::Context,
        execution_engine::ExecutionEngine,
        module::{Linkage, Module},
        passes::{PassManager, PassManagerBuilder},
        types::{
            AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType,
        },
        values::{
            BasicMetadataValueEnum, BasicValue, BasicValueEnum, CallSiteValue, FunctionValue,
            PointerValue,
        },
        AddressSpace, OptimizationLevel,
    };
}

//

pub struct Engine {
    module: llvm::Module<'static>,
    builder: llvm::Builder<'static>,

    fpm: llvm::PassManager<llvm::FunctionValue<'static>>,

    ee: llvm::ExecutionEngine<'static>,

    fns: RefCell<HashMap<Substr, llvm::FunctionValue<'static>>>,
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
        let id = ArcStr::from(base_name).substr(..);
        self.load_fndef(&FnDef {
            proto: FnProto {
                id: id.clone(),
                params: ParamList(vec![Param {
                    id: arcstr::format!("1").into(),
                    ty: Type::I32,
                }]),
                ty: Type::None,
            },
            block: Block {
                stmts: vec![Stmt::Expr(Expr::FnCallExt(FnCallExt {
                    id,
                    addr: ptr as _,
                    args: ArgList(vec![Expr::Variable(arcstr::format!("1").into())]),
                }))],
            },
        });

        /* self.load_fnproto(); */
    }

    pub fn run(&self, f: &str) {
        // TODO: validate

        let start = unsafe {
            self.ee
                .get_function::<unsafe extern "C" fn() -> ()>(f)
                .unwrap()
        };

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
        let fnproto = &fndef.proto;
        let proto = self.load_fndecl(fnproto);

        let entry = self
            .module
            .get_context()
            .append_basic_block(proto, "fn-entry");
        self.builder.position_at_end(entry);

        let mut scope = Scope::new(entry);

        for (arg, param) in proto.get_param_iter().zip(fnproto.params.0.iter()) {
            let ty = arg.get_type();
            let ptr = self.entry_var_alloca(&param.id, ty, &scope);
            self.builder.build_store(ptr, arg);

            scope.set(param.id.clone(), (ty, ptr));
        }

        self.load_block(&fndef.block, &mut scope);
        self.load_return(&Return(None), &mut scope);

        if !proto.verify(true) {
            panic!("invalid function");
        }

        self.fpm.run_on(&proto);

        proto
    }

    pub fn load_fndecl(&self, fnproto: &FnProto) -> llvm::FunctionValue<'static> {
        let proto = self.load_fnproto(fnproto);

        self.fns.borrow_mut().insert(fnproto.id.clone(), proto);

        proto
    }

    pub fn load_fnproto(&self, fnproto: &FnProto) -> llvm::FunctionValue<'static> {
        let FnProto { id, params, ty } = fnproto;

        // let name = ProtoName::from_fnproto(fnproto);
        // let name_str = format!("{}", ProtoName::from_fnproto(fnproto));
        let name = &id;
        // println!("proto: `{name}`");

        let ty = Self::load_fntype(ty, params.0.iter().map(|p| &p.ty));

        let proto = self.module.add_function(name, ty, None);

        for (arg, param) in proto.get_param_iter().zip(fnproto.params.0.iter()) {
            arg.set_name(&param.id);
        }

        proto
    }

    pub fn load_block(&self, block: &Block, scope: &mut Scope) {
        for stmt in block.stmts.iter() {
            self.load_stmt(stmt, scope);
        }
    }

    pub fn load_fntype<'a>(
        ty: &Type,
        param_types: impl Iterator<Item = &'a Type>,
    ) -> llvm::FunctionType<'static> {
        let param_types: Vec<llvm::BasicMetadataTypeEnum> =
            param_types.map(|t| Self::type_enum(t).into()).collect();

        let ctx = get_ctx();
        match ty {
            Type::I32 => ctx.i32_type().fn_type(&param_types, false),
            Type::F32 => ctx.f32_type().fn_type(&param_types, false),
            Type::Bool => ctx.bool_type().fn_type(&param_types, false),
            Type::None => ctx.void_type().fn_type(&param_types, false),
        }
    }

    pub fn load_stmt(&self, stmt: &Stmt, scope: &mut Scope) {
        match stmt {
            Stmt::Return(ret) => self.load_return(ret, scope),
            Stmt::Let(r#let) => self.load_let(r#let, scope),
            Stmt::Assign(assign) => self.load_assign(assign, scope),
            Stmt::CtrlIf(ctrlif) => todo!(),
            Stmt::Expr(expr) => _ = self.load_expr(expr, scope),
            /* Stmt::FnCall(fncall) => _ = self.load_fncall(fncall, scope),
            Stmt::FnCallExt(fncallext) => _ = self.load_fncallext(fncallext, scope), */
        }
    }

    pub fn load_return(&self, ret: &Return, scope: &mut Scope) {
        let val = ret.0.as_ref().map(|expr| self.load_expr(expr, scope));
        let val = val.as_ref().map(|val| val as _);

        self.builder.build_return(val);
    }

    pub fn load_let(&self, r#let: &Let, scope: &mut Scope) {
        let val = self.load_expr(&r#let.expr, scope);

        let ty = val.get_type();
        let ptr = self.entry_var_alloca(&r#let.id, ty, scope);
        self.builder.build_store(ptr, val);

        scope.set(r#let.id.clone(), (ty, ptr));
    }

    pub fn load_assign(&self, assign: &Assign, scope: &mut Scope) {
        let val = self.load_expr(&assign.expr, scope);

        let (ty, ptr) = scope.get(&assign.id);
        assert_eq!(val.get_type(), ty);

        let val = 'op: {
            let op = match assign.op {
                AssignOp::Assign => {
                    break 'op val;
                }
                AssignOp::Add => BinOp::Add,
                AssignOp::Sub => BinOp::Sub,
                AssignOp::Mul => BinOp::Mul,
                AssignOp::Div => BinOp::Div,
            };

            let target = self.builder.build_load(ty, ptr, "tmp-assign-load");
            self.load_binexpr(target, val, op)
        };

        self.builder.build_store(ptr, val);
    }

    pub fn load_fncall(&self, fncall: &FnCall, scope: &mut Scope) -> llvm::BasicValueEnum<'static> {
        let func = *self.fns.borrow().get(&fncall.id).expect("unknown function");

        let args: Vec<llvm::BasicMetadataValueEnum> = fncall
            .args
            .0
            .iter()
            .map(|expr| self.load_expr(expr, scope).into())
            .collect();

        let call = self.builder.build_call(func, &args, "fncall-jit");

        Self::process_call_value(call)
    }

    pub fn load_fncallext(
        &self,
        fncallext: &FnCallExt,
        scope: &mut Scope,
    ) -> llvm::BasicValueEnum<'static> {
        let ty = self
            .fns
            .borrow()
            .get(&fncallext.id)
            .expect("unknown function")
            .get_type();

        let args: Vec<llvm::BasicMetadataValueEnum> = fncallext
            .args
            .0
            .iter()
            .map(|expr| self.load_expr(expr, scope).into())
            .collect();

        let int_ty = get_ctx().ptr_sized_int_type(self.ee.get_target_data(), None);
        let func = self.builder.build_int_to_ptr(
            int_ty.const_int(fncallext.addr as _, false),
            int_ty.ptr_type(llvm::AddressSpace::default()),
            "tmp-int-to-ptr",
        );

        let call = self
            .builder
            .build_indirect_call(ty, func, &args, "fncall-ext");

        Self::process_call_value(call)
    }

    pub fn load_expr(&self, expr: &Expr, scope: &mut Scope) -> llvm::BasicValueEnum<'static> {
        match expr {
            Expr::BinExpr { sides, op } => {
                let lhs = self.load_expr(&sides.0, scope);
                let rhs = self.load_expr(&sides.1, scope);

                self.load_binexpr(lhs, rhs, *op)
            }
            Expr::Value(value) => self.load_value(value),
            Expr::Variable(variable) => self.load_variable(variable, scope),
            Expr::FnCall(fncall) => self.load_fncall(fncall, scope),
            Expr::FnCallExt(fncallext) => self.load_fncallext(fncallext, scope),
        }
    }

    pub fn load_binexpr(
        &self,
        lhs: llvm::BasicValueEnum<'static>,
        rhs: llvm::BasicValueEnum<'static>,
        op: BinOp,
    ) -> llvm::BasicValueEnum<'static> {
        match (lhs, rhs, op) {
            (
                llvm::BasicValueEnum::IntValue(lhs),
                llvm::BasicValueEnum::IntValue(rhs),
                BinOp::Add,
            ) => self
                .builder
                .build_int_add(lhs, rhs, "int-add")
                .as_basic_value_enum(),

            (
                llvm::BasicValueEnum::IntValue(lhs),
                llvm::BasicValueEnum::IntValue(rhs),
                BinOp::Sub,
            ) => self
                .builder
                .build_int_sub(lhs, rhs, "int-sub")
                .as_basic_value_enum(),

            (
                llvm::BasicValueEnum::IntValue(lhs),
                llvm::BasicValueEnum::IntValue(rhs),
                BinOp::Mul,
            ) => self
                .builder
                .build_int_mul(lhs, rhs, "int-mul")
                .as_basic_value_enum(),

            (
                llvm::BasicValueEnum::IntValue(lhs),
                llvm::BasicValueEnum::IntValue(rhs),
                BinOp::Div,
            ) => self
                .builder
                .build_int_signed_div(lhs, rhs, "int-div")
                .as_basic_value_enum(),

            (lhs, rhs, op) => {
                panic!("cannot eval `{op:?}` with `{lhs}` and `{rhs}`");
            }
        }
    }

    pub fn load_value(&self, value: &Value) -> llvm::BasicValueEnum<'static> {
        let ctx = get_ctx();
        match value {
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

    pub fn load_variable(&self, variable: &str, scope: &Scope) -> llvm::BasicValueEnum<'static> {
        let (ty, ptr) = scope.get(variable);
        self.builder.build_load(ty, ptr, "tmp-load")
    }

    fn process_call_value(call: llvm::CallSiteValue<'static>) -> llvm::BasicValueEnum<'static> {
        if let Some(val) = call.try_as_basic_value().left() {
            val
        } else {
            get_ctx()
                .struct_type(&[], false)
                .const_zero()
                .as_basic_value_enum()
        }
    }

    fn entry_var_alloca(
        &self,
        name: &str,
        // ty: &Type,
        ty: llvm::BasicTypeEnum<'static>,
        scope: &Scope,
    ) -> llvm::PointerValue<'static> {
        // TODO: reuse
        scope
            .entry_builder
            .build_alloca(ty, /* Self::type_enum(ty) */ name)
    }

    fn type_enum(ty: &Type) -> llvm::BasicTypeEnum<'static> {
        let ctx = get_ctx();
        match ty {
            Type::I32 => ctx.i32_type().into(),
            Type::F32 => ctx.f32_type().into(),
            Type::Bool => ctx.bool_type().into(),
            Type::None => ctx.struct_type(&[], false).into(),
        }
    }
}

impl Default for Engine {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Scope {
    entry_builder: llvm::Builder<'static>,
    vars: Vec<Vec<Var>>,
}

impl Scope {
    pub fn new(entry: llvm::BasicBlock<'static>) -> Self {
        let entry_builder = get_ctx().create_builder();

        if let Some(first) = entry.get_first_instruction() {
            entry_builder.position_before(&first);
        } else {
            entry_builder.position_at_end(entry);
        }

        Self {
            entry_builder,
            vars: vec![],
        }
    }

    pub fn push(&mut self) {
        self.vars.push(vec![]);
    }

    pub fn pop(&mut self) {
        self.vars.pop();
    }

    pub fn set(
        &mut self,
        id: Substr,
        val: (llvm::BasicTypeEnum<'static>, llvm::PointerValue<'static>),
    ) {
        let last = self.last_mut();
        last.push(Var { id, val });
    }

    /* pub fn assign(&mut self, id: &str, val: llvm::BasicValueEnum<'static>) {
        self.vars
            .iter_mut()
            .flat_map(|v| v.iter_mut())
            .rev()
            .find(|var| var.id == id)
            .expect("variable not found")
            .val = val;
    } */

    pub fn get(&self, id: &str) -> (llvm::BasicTypeEnum<'static>, llvm::PointerValue<'static>) {
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
    val: (llvm::BasicTypeEnum<'static>, llvm::PointerValue<'static>),
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
