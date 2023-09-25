use std::ops::Deref;

use arcstr::ArcStr;
use inkwell::{
    module::Linkage,
    types::{AnyType, AnyTypeEnum, BasicMetadataTypeEnum, BasicType, FunctionType},
    values::FunctionValue,
};
use once_cell::unsync::Lazy;

use crate::{
    syntax::{FnDef, FnProto, Item, Module, Param, ParamList, Type},
    util::IterList,
};

//

pub mod llvm {
    pub use inkwell::{context::Context, module::Module};
}

//

pub struct Engine {
    module: llvm::Module<'static>,
}

impl Engine {
    pub fn new() -> Self {
        Self {
            module: get_ctx().create_module("<src>"),
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
================
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
        // let proto =
    }

    pub fn load_fnproto(&self, fnproto: &FnProto) -> FunctionValue<'static> {
        let FnProto { id, params, ty } = fnproto;

        let name_params = IterList(params.0.iter().map(|v| v.ty.as_str()));
        let name_ty = ty.as_str();

        let name = format!("{id} {name_params}->{name_ty}");
        println!("proto: `{name}`");

        let param_types: Vec<BasicMetadataTypeEnum> = params
            .0
            .iter()
            .map(|p| Self::load_type(p.ty.clone()))
            .collect();
        let ty = Self::load_fntype(ty.clone(), &param_types[..]);

        self.module.add_function(&name, ty, None)
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
}

impl Default for Engine {
    fn default() -> Self {
        Self::new()
    }
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
