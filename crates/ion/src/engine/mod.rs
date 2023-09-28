use std::{cell::RefCell, collections::HashMap, mem};

use arcstr::Substr;
use inkwell::values::BasicValueEnum;
use once_cell::unsync::{Lazy, OnceCell};

use crate::{
    syntax::{
        Assign, AssignOp, BinOp, Block, CtrlFor, CtrlIf, Expr, FnCall, FnCallExt, FnDef, FnProto,
        Item, Let, Module, Param, ParamList, RangeKind, Return, Stmt, Type, Value,
    },
    IonCallback, OptLevel,
};

use llvm::BasicValue;

//

mod callback;

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
            IntValue, PointerValue, StructValue,
        },
        AddressSpace, IntPredicate, OptimizationLevel,
    };
}

//

impl OptLevel {
    const fn llvm_opt_level(self) -> llvm::OptimizationLevel {
        match self {
            OptLevel::High => llvm::OptimizationLevel::Aggressive,
            OptLevel::Medium => llvm::OptimizationLevel::Default,
            OptLevel::Low => llvm::OptimizationLevel::Less,
            OptLevel::None => llvm::OptimizationLevel::None,
        }
    }
}

pub struct Engine {
    module: llvm::Module<'static>,

    opt_level: OptLevel,
    mpm: llvm::PassManager<llvm::Module<'static>>,
    fpm: llvm::PassManager<llvm::FunctionValue<'static>>,
    ee: OnceCell<llvm::ExecutionEngine<'static>>,

    fns: RefCell<HashMap<Substr, llvm::FunctionValue<'static>>>,
    // user_fns: Vec<*mut c_void>,
}

impl Engine {
    pub fn new(opt_level: OptLevel) -> Self {
        let ctx = get_ctx();
        let module = ctx.create_module("<src>");

        let (mpm, fpm) = Self::gen_pm(&module, opt_level);
        let ee = OnceCell::new();

        Self {
            module,

            opt_level,
            mpm,
            fpm,
            ee,

            fns: RefCell::new(HashMap::new()),
            // user_fns: vec![],
        }
    }

    pub fn set_opt_level(&mut self, opt_level: OptLevel) {
        self.opt_level = opt_level;
        (self.mpm, self.fpm) = Self::gen_pm(&self.module, opt_level);
    }

    fn gen_pm(
        module: &llvm::Module<'static>,
        opt_level: OptLevel,
    ) -> (
        llvm::PassManager<llvm::Module<'static>>,
        llvm::PassManager<llvm::FunctionValue<'static>>,
    ) {
        let fpmb = llvm::PassManagerBuilder::create();
        fpmb.set_optimization_level(opt_level.llvm_opt_level());
        fpmb.set_inliner_with_threshold(1024);

        // let lpm = llvm::PassManager::create(&());
        let mpm = llvm::PassManager::create(&());
        let fpm = llvm::PassManager::create(module);

        // fpmb.populate_lto_pass_manager(&lpm, true, true);
        fpmb.populate_module_pass_manager(&mpm);
        fpmb.populate_function_pass_manager(&fpm);
        fpm.initialize();

        (mpm, fpm)
    }

    fn ee(&self) -> &llvm::ExecutionEngine<'static> {
        self.ee.get_or_init(|| {
            self.module
                .create_jit_execution_engine(self.opt_level.llvm_opt_level())
                .unwrap()
        })
    }

    pub fn dump_ir(&self) -> String {
        self.module.print_to_string().to_string()
    }

    pub fn add<C, F>(&self, base_name: &str, func: C)
    where
        C: IonCallback<F> + 'static,
    {
        tracing::debug!(
            "adding closure (`{base_name}`{:?}->{:?}) to runtime",
            func.args(),
            func.ty()
        );

        // TODO: re-add the old 'extern "C" fn callbacks'
        // as optional, faster, lower overhead callbacks
        let params: Vec<Param> = func
            .args()
            .iter()
            .enumerate()
            .map(|(i, ty)| Param {
                id: arcstr::format!("{i}").into(),
                ty: ty.clone(),
            })
            .collect();

        let fnproto = FnProto {
            id: base_name.into(),
            params: ParamList(params),
            ty: func.ty(),
        };
        let wrapper = self.load_fnproto(&fnproto);

        let mut scope = Scope::new(wrapper);
        scope.init_fn_args(self, &fnproto);

        let result = callback::call_closure(self, &scope, func);
        let result = result.as_ref().map(|r| r as _);
        scope.builder.build_return(result);

        self.fns.borrow_mut().insert(fnproto.id.clone(), wrapper);
    }

    pub fn run(&self, f: &str) {
        // TODO: validate

        tracing::debug!("running");
        let fns = self.fns.borrow();
        let func = fns.get(f).expect("unknown function");

        self.mpm.run_on(&self.module);
        self.fpm.run_on(func);

        let start = unsafe {
            self.ee()
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

        let mut scope = Scope::new(proto);
        scope.init_fn_args(self, fnproto);

        self.load_block(&fndef.block, &mut scope);

        if fndef.proto.ty == Type::None {
            self.load_return(&Return(None), &mut scope);
        }

        if !proto.verify(true) {
            println!("\nIR:\n{}\nEND IR", self.dump_ir());
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
            Type::U64 => ctx.i64_type().fn_type(&param_types, false),
            Type::I32 => ctx.i32_type().fn_type(&param_types, false),
            Type::F32 => ctx.f32_type().fn_type(&param_types, false),
            Type::Bool => ctx.bool_type().fn_type(&param_types, false),
            Type::None => ctx.void_type().fn_type(&param_types, false),
        }
    }

    pub fn load_stmt(&self, stmt: &Stmt, scope: &mut Scope) {
        match stmt {
            Stmt::Return(ret) => self.load_return(ret, scope),
            Stmt::Let(r#let) => _ = self.load_let(r#let, scope),
            Stmt::Assign(assign) => _ = self.load_assign(assign, scope),
            Stmt::CtrlIf(ctrlif) => self.load_ctrlif(ctrlif, scope),
            Stmt::CtrlFor(ctrlfor) => self.load_ctrlfor(ctrlfor, scope),
            Stmt::Expr(expr) => _ = self.load_expr(expr, scope),
            Stmt::Semi => {} /* Stmt::FnCall(fncall) => _ = self.load_fncall(fncall, scope),
                             Stmt::FnCallExt(fncallext) => _ = self.load_fncallext(fncallext, scope), */
        }
    }

    pub fn load_return(&self, ret: &Return, scope: &mut Scope) {
        let val = ret.0.as_ref().map(|expr| self.load_expr(expr, scope));
        let val = val.as_ref().map(|val| val as _);

        scope.builder.build_return(val);
    }

    pub fn load_let(&self, r#let: &Let, scope: &mut Scope) -> llvm::PointerValue<'static> {
        let val = self.load_expr(&r#let.expr, scope);
        self.load_let_raw(r#let.id.clone(), val, scope)
    }

    fn load_let_raw(
        &self,
        id: Substr,
        val: llvm::BasicValueEnum<'static>,
        scope: &mut Scope,
    ) -> llvm::PointerValue<'static> {
        let ty = val.get_type();
        // stack alloc for the variable
        let ptr = self.entry_var_alloca(&id, ty, scope);
        // save `val` to the alloc
        scope.builder.build_store(ptr, val);
        // save the variable handle
        scope.set(id, (ty, ptr));
        ptr
    }

    pub fn load_assign(&self, assign: &Assign, scope: &mut Scope) -> llvm::PointerValue<'static> {
        let val = self.load_expr(&assign.expr, scope);
        self.load_assign_raw(&assign.id, val, assign.op, scope)
    }

    fn load_assign_raw(
        &self,
        id: &str,
        val: llvm::BasicValueEnum<'static>,
        op: AssignOp,
        scope: &mut Scope,
    ) -> llvm::PointerValue<'static> {
        let (ty, ptr) = scope.get(id);
        assert_eq!(val.get_type(), ty);

        let val = 'op: {
            let op = match op {
                AssignOp::Assign => {
                    break 'op val;
                }
                AssignOp::Add => BinOp::Add,
                AssignOp::Sub => BinOp::Sub,
                AssignOp::Mul => BinOp::Mul,
                AssignOp::Div => BinOp::Div,
            };

            let target = scope.builder.build_load(ty, ptr, "tmp-assign-load");
            self.load_binexpr(target, val, op, scope)
        };

        scope.builder.build_store(ptr, val);
        ptr
    }

    pub fn load_ctrlif(&self, ctrlif: &CtrlIf, scope: &mut Scope) {
        let condition = self.load_expr(&ctrlif.condition, scope);

        let BasicValueEnum::IntValue(condition) = condition else {
            panic!("invalid if condition");
        };

        let if_true = get_ctx().append_basic_block(scope.proto, "if-true");
        // let if_false = get_ctx().append_basic_block(scope.proto, "if-false");
        let if_continue = get_ctx().append_basic_block(scope.proto, "if-continue");

        // scope.builder.build_float_compare(op, lhs, rhs, name);

        scope
            .builder
            .build_conditional_branch(condition, if_true, if_continue /* if_false */);

        // if_true block
        scope.builder.position_at_end(if_true);
        self.load_block(&ctrlif.block, scope);
        scope.builder.build_unconditional_branch(if_continue);

        // if_continue block
        scope.builder.position_at_end(if_continue);
    }

    pub fn load_ctrlfor(&self, ctrlfor: &CtrlFor, scope: &mut Scope) {
        let from = self.load_expr(&ctrlfor.range.range.0, scope);
        let to = self.load_expr(&ctrlfor.range.range.1, scope);

        // assert_ne!(from.get_type(), to.get_type());

        let cmp = match ctrlfor.range.kind {
            RangeKind::Closed => BinOp::Le,
            RangeKind::Open => BinOp::Lt,
        };

        let ctx = get_ctx();
        let for_loop = ctx.append_basic_block(scope.proto, "for-loop");
        let for_block = ctx.append_basic_block(scope.proto, "for-block");
        let for_continue = ctx.append_basic_block(scope.proto, "for-continue");

        // for (***int i = 0***; i<10; i++) {}
        // it obv doesn't have to be named `i`
        self.load_let_raw(ctrlfor.id.clone(), from, scope);
        // jump to the loop
        scope.builder.build_unconditional_branch(for_loop);

        // for-loop block
        scope.builder.position_at_end(for_loop);

        // for (int i = 0; ***i<10***; i++) {}
        let i_now = self.load_variable(&ctrlfor.id, scope);
        let cmp = self.load_binexpr(i_now, to, cmp, scope);
        let BasicValueEnum::IntValue(cmp) = cmp else {
            panic!("invalid for-if condition");
        };
        scope
            .builder
            .build_conditional_branch(cmp, for_block, for_continue);

        // for-block block
        scope.builder.position_at_end(for_block);

        // for (int i = 0; i<10; i++) ***{}***
        self.load_block(&ctrlfor.block, scope);

        // for (int i = 0; i<10; ******)
        self.load_assign_raw(
            &ctrlfor.id,
            self.load_value(&Value::Int(1)),
            AssignOp::Add,
            scope,
        );
        scope.builder.build_unconditional_branch(for_loop);

        // for-after block
        scope.builder.position_at_end(for_continue);
    }

    pub fn load_fncall(&self, fncall: &FnCall, scope: &mut Scope) -> llvm::BasicValueEnum<'static> {
        let func = *self.fns.borrow().get(&fncall.id).expect("unknown function");

        let args: Vec<llvm::BasicMetadataValueEnum> = fncall
            .args
            .0
            .iter()
            .map(|expr| self.load_expr(expr, scope).into())
            .collect();

        let call = scope.builder.build_call(func, &args, "fncall-jit");

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

        let int_ty = get_ctx().ptr_sized_int_type(self.ee().get_target_data(), None);
        let func = scope.builder.build_int_to_ptr(
            int_ty.const_int(fncallext.addr as _, false),
            int_ty.ptr_type(llvm::AddressSpace::default()),
            "tmp-int-to-ptr",
        );

        let call = scope
            .builder
            .build_indirect_call(ty, func, &args, "fncall-ext");

        Self::process_call_value(call)
    }

    pub fn load_expr(&self, expr: &Expr, scope: &mut Scope) -> llvm::BasicValueEnum<'static> {
        match expr {
            Expr::BinExpr { sides, op } => {
                let lhs = self.load_expr(&sides.0, scope);
                let rhs = self.load_expr(&sides.1, scope);

                self.load_binexpr(lhs, rhs, *op, scope)
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
        scope: &Scope,
    ) -> llvm::BasicValueEnum<'static> {
        use llvm::{BasicValueEnum::IntValue as I, IntPredicate as Ip};

        let b = &scope.builder;

        fn conv<V: llvm::BasicValue<'static>>(v: V) -> llvm::BasicValueEnum<'static> {
            v.as_basic_value_enum()
        }

        fn bin_op_as_int_predicate(op: BinOp) -> Option<Ip> {
            match op {
                BinOp::Lt => Some(Ip::SLT),
                BinOp::Le => Some(Ip::SLE),
                BinOp::Eq => Some(Ip::EQ),
                BinOp::Ge => Some(Ip::SGE),
                BinOp::Gt => Some(Ip::SGT),
                _ => None,
            }
        }

        match (lhs, rhs, op, bin_op_as_int_predicate(op)) {
            (I(l), I(r), BinOp::Add, _) => conv(b.build_int_add(l, r, "int-add")),
            (I(l), I(r), BinOp::Sub, _) => conv(b.build_int_sub(l, r, "int-sub")),
            (I(l), I(r), BinOp::Mul, _) => conv(b.build_int_mul(l, r, "int-mul")),
            (I(l), I(r), BinOp::Div, _) => conv(b.build_int_signed_div(l, r, "int-div")),

            (I(l), I(r), _, Some(cmp)) => conv(b.build_int_compare(cmp, l, r, "int-cmp")),

            (lhs, rhs, op, _) => {
                let (lhs, rhs) = (lhs.get_type(), rhs.get_type());
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
        scope.builder.build_load(ty, ptr, "tmp-load")
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
        let block = scope
            .proto
            .get_first_basic_block()
            .expect("scope without a block");

        if let Some(first) = block.get_first_instruction() {
            scope.entry_builder.position_before(&first);
        } else {
            scope.entry_builder.position_at_end(block);
        }

        scope
            .entry_builder
            .build_alloca(ty, /* Self::type_enum(ty) */ name)
    }

    fn type_enum(ty: &Type) -> llvm::BasicTypeEnum<'static> {
        let ctx = get_ctx();
        match ty {
            Type::U64 => ctx.i64_type().into(),
            Type::I32 => ctx.i32_type().into(),
            Type::F32 => ctx.f32_type().into(),
            Type::Bool => ctx.bool_type().into(),
            Type::None => ctx.struct_type(&[], false).into(),
        }
    }
}

impl Default for Engine {
    fn default() -> Self {
        Self::new(OptLevel::default())
    }
}

pub struct Scope {
    proto: llvm::FunctionValue<'static>,
    entry_builder: llvm::Builder<'static>,
    builder: llvm::Builder<'static>,

    vars: Vec<Vec<Var>>,
}

impl Scope {
    pub fn new(proto: llvm::FunctionValue<'static>) -> Self {
        let entry = get_ctx().append_basic_block(proto, "fn-entry");

        let entry_builder = get_ctx().create_builder();
        let builder = get_ctx().create_builder();

        entry_builder.position_at_end(entry);
        builder.position_at_end(entry);

        Self {
            proto,
            entry_builder,
            builder,

            vars: vec![],
        }
    }

    pub fn init_fn_args(&mut self, engine: &Engine, fnproto: &FnProto) {
        for (arg, param) in self.proto.get_param_iter().zip(fnproto.params.0.iter()) {
            let ty = arg.get_type();
            let ptr = engine.entry_var_alloca(&param.id, ty, self);
            self.builder.build_store(ptr, arg);

            self.set(param.id.clone(), (ty, ptr));
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
            .unwrap_or_else(|| panic!("variable `{id}` not found"))
            .val
    }

    fn last_mut(&mut self) -> &mut Vec<Var> {
        if self.vars.is_empty() {
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
