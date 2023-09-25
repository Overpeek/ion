use std::{borrow::Borrow, collections::HashMap, fmt, io::Write, mem, ops::Deref, rc::Rc};

use arcstr::{ArcStr, Substr};
use inkwell::{
    module::Linkage,
    types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, FunctionType},
    values::{BasicValue, BasicValueEnum, FunctionValue},
};
use once_cell::unsync::Lazy;

use crate::{
    syntax::{
        BinOp, Block, Expr, FnDef, FnProto, Item, Module, Param, ParamList, Return, Stmt, Type,
        Value,
    },
    util::IterList,
};

//

pub mod llvm {
    pub use inkwell::{
        builder::Builder, context::Context, module::Module, types::BasicTypeEnum,
        values::BasicValueEnum,
    };
}

//

pub fn proto_name(base: &str, args: &[Type], ret: Type) {}

//

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
}

pub struct Engine {
    module: llvm::Module<'static>,
    builder: llvm::Builder<'static>,

    fns: HashMap<ProtoName, FunctionValue<'static>>,
}

impl Engine {
    pub fn new() -> Self {
        let ctx = get_ctx();
        let module = ctx.create_module("<src>");
        let builder = ctx.create_builder();

        Self {
            module,
            builder,
            fns: HashMap::new(),
        }
    }

    pub fn add(&self, base_name: &str, ptr: fn(i32)) {
        self.load_fnproto(&FnProto {
            id: ArcStr::from(base_name).into(),
            params: ParamList(vec![Param {
                id: ArcStr::new().into(),
                ty: Type::I32,
            }]),
            ty: Type::None,
        });

        if let Ok(ir) = self.module.print_to_string().to_str() {
            println!(
                r#"
===============
 BEGIN LLVM-IR:
================
{ir}
================
 END LLVM-IR
================"#
            );
        } else {
            println!("LLVM-IR contains invalid utf8")
        }
    }

    pub fn load_module(&self, module: &Module) {
        for item in module.items.iter() {
            self.load_item(item)
        }
    }

    pub fn load_item(&self, item: &Item) {
        match item {
            Item::FnDef(fndef) => self.load_fndef(fndef),
        }
    }

    pub fn load_fndef(&self, fndef: &FnDef) {
        let proto = self.load_fnproto(&fndef.proto);

        let mut scope = Scope::new();

        let entry = self
            .module
            .get_context()
            .append_basic_block(proto, "fn-entry");
        self.builder.position_at_end(entry);

        for (arg, param) in proto.get_param_iter().zip(fndef.proto.params.0.iter()) {
            scope.assign(&param.id, arg);
        }

        self.load_block(&fndef.block, &mut scope);
    }

    pub fn load_fnproto(&self, fnproto: &FnProto) -> FunctionValue<'static> {
        let FnProto { id, params, ty } = fnproto;

        let name = ProtoName::from_fnproto(fnproto);
        let name_str = format!("{}", ProtoName::from_fnproto(fnproto));
        println!("proto: `{name_str}`");

        let param_types: Vec<BasicMetadataTypeEnum> = params
            .0
            .iter()
            .map(|p| Self::load_type(p.ty.clone()))
            .collect();
        let ty = Self::load_fntype(ty.clone(), &param_types[..]);

        let proto = self.module.add_function(&name_str, ty, None);

        for (arg, param) in proto.get_param_iter().zip(params.0.iter()) {
            arg.set_name(&param.id);
        }

        self.fns.insert(name, proto);

        proto
    }

    pub fn load_block(&self, block: &Block, scope: &mut Scope) {
        for stmt in block.stmts.iter() {
            self.load_stmt(stmt, scope);
        }
    }

    pub fn load_fntype(
        ty: Type,
        param_types: &[BasicMetadataTypeEnum<'static>],
    ) -> FunctionType<'static> {
        let ctx = get_ctx();
        match ty {
            Type::I32 => ctx.i32_type().fn_type(param_types, false),
            Type::F32 => ctx.f32_type().fn_type(param_types, false),
            Type::None => ctx.void_type().fn_type(param_types, false),
        }
    }

    pub fn load_type(ty: Type) -> BasicMetadataTypeEnum<'static> {
        let ctx = get_ctx();
        match ty {
            Type::I32 => ctx.i32_type().into(),
            Type::F32 => ctx.f32_type().into(),
            Type::None => ctx.struct_type(&[], false).into(),
        }
    }

    pub fn load_stmt(&self, stmt: &Stmt, scope: &mut Scope) {
        match stmt {
            Stmt::Return(v) => todo!(),
            Stmt::Let(v) => todo!(),
            Stmt::FnCall(v) => todo!(),
        }
    }

    pub fn load_return(&self, ret: &Return, scope: &mut Scope) {
        let val = self.load_expr(&ret.0, scope);
        self.builder.build_return(Some(&val));
    }

    pub fn load_expr(&self, expr: &Expr, scope: &mut Scope) -> BasicValueEnum<'static> {
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
            Expr::Variable(v) => scope.get(&v),
            Expr::FnCall(v) => {
                v.args;

                self.fns.get(&ProtoName::new(base, params, ty));

                self.builder.build_call(function, args, name);
                todo!()
            }
        }
    }
}

impl Default for Engine {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Scope<'a> {
    vars: Vec<Vec<Var<'a>>>,
}

impl<'a> Scope<'a> {
    pub const fn new() -> Self {
        Self { vars: vec![] }
    }

    pub fn push(&mut self) {
        self.vars.push(vec![]);
    }

    pub fn pop(&mut self) {
        self.vars.pop();
    }

    pub fn assign(&mut self, id: &'a str, val: BasicValueEnum<'static>) {
        let last = self.last_mut();

        last.push(Var { id, val });
    }

    pub fn set(&mut self, id: &'a str, val: BasicValueEnum<'static>) {
        self.vars
            .iter_mut()
            .flat_map(|v| v.iter_mut())
            .rev()
            .find(|var| var.id == id)
            .expect("variable not found")
            .val = val;
    }

    pub fn get(&self, id: &'a str) -> BasicValueEnum<'static> {
        self.vars
            .iter()
            .flat_map(|v| v.iter())
            .rev()
            .find(|var| var.id == id)
            .expect("variable not found")
            .val
    }

    fn last_mut(&mut self) -> &'_ mut Vec<Var<'a>> {
        if self.vars.len() == 0 {
            self.vars.push(vec![]);
        }

        self.vars.last_mut().unwrap()
    }
}

struct Var<'a> {
    id: &'a str,
    val: BasicValueEnum<'static>,
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
