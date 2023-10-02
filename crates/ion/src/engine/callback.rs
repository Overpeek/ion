use std::{ffi::c_void, slice::from_raw_parts_mut};

use crate::{
    val::{AsIonType, RuntimeValue},
    IonCallback, Type,
};

use super::{get_ctx, llvm, Engine, Scope};

//

pub type Callback = dyn FnMut(&[RuntimeValue]) -> RuntimeValue + 'static;

//

struct UnionType {
    size: u32,
    align: u32,
}

pub fn union_type(variants: &[Type]) {
    for variant in variants {}
}

pub fn call_closure<C, F>(
    engine: &Engine,
    scope: &Scope,
    func: C,
) -> Option<llvm::BasicValueEnum<'static>>
where
    C: IonCallback<F> + 'static,
{
    let ty = func.ty();
    let ctx = get_ctx();

    // TODO: lazy + once + abstracted

    let runtime_value_type = ctx.get_struct_type("RuntimeValue").unwrap_or_else(|| {
        let u8_type = ctx.i8_type();
        let s = ctx.opaque_struct_type("RuntimeValue");
        s.set_body(
            &[
                u8_type.into(),
                u8_type
                    .array_type(RuntimeValue::size_of() as u32 - 1)
                    .into(),
            ],
            false,
        );
        s
    });

    engine.entry_var_alloca(name, ty, scope, alignment)

    // TODO: implement real structs and arrays
    // this basically calls the __ion_callback_handler with correct arguments
    // (array length + array pointer)
    // (each array value is a struct { union { u64, i32, f32, bool, struct {} }, u8 })

    let ty_u64 = ctx.i64_type();
    let ty_u32 = ctx.i32_type();
    let ty_f32 = ctx.f32_type();
    let ty_u8 = ctx.i8_type();
    let ty_bool = ctx.bool_type();
    let ty_str = ctx.struct_type(&[ty_u64.into(), ty_u64.into()], false);
    let tag = ty_u8;

    let runtime_type = ctx.struct_type(
        &[
            ty_u8.into(),
            ty_u8.array_type(RuntimeValue::size_of() as u32 - 1).into(),
        ],
        false,
    );

    let runtime_type_str = ctx.struct_type(
        &[
            ty_u64.array_type(1).into(),
            ctx.struct_type(&[ty_u64.into(), ty_u64.into()], false)
                .into(),
        ],
        false,
    );
    let runtime_type_u64 = ctx.struct_type(
        &[
            ty_u64.array_type(1).into(),
            ty_u64.into(),
            // last 64 bits can be garbage
        ],
        false,
    );
    let runtime_type_i32 = ctx.struct_type(
        &[
            ty_u32.array_type(2).into(),
            ty_u32.into(),
            ty_u32.array_type(1).into(),
            // last 64 bits can be garbage
        ],
        false,
    );
    let runtime_type_f32 = ctx.struct_type(
        &[
            ty_u32.array_type(2).into(),
            ty_f32.into(),
            ty_u32.array_type(1).into(),
            // last 64 bits can be garbage
        ],
        false,
    );
    let runtime_type_bool = ctx.struct_type(
        &[
            ty_u8.array_type(8).into(),
            ty_u8.into(),
            ty_u8.array_type(7).into(),
            // last 64 bits can be garbage
        ],
        false,
    );
    let runtime_type_none = ctx.struct_type(&[tag.into()], false);

    let array_runtime_type = runtime_type.array_type(func.args().len() as u32 + 1);

    let array = engine.entry_var_alloca(
        "argv",
        array_runtime_type.into(),
        scope,
        Some(RuntimeValue::align_of() as u32),
    );
    let elem = engine.entry_var_alloca(
        "elem",
        runtime_type.into(),
        scope,
        Some(RuntimeValue::align_of() as u32),
    );

    return None;

    for (i, arg) in func.args().iter().enumerate() {
        let v = engine.load_variable(&format!("_arg_{i}"), scope);
        let tag_v = RuntimeValue::tag(arg) as u64;

        assert!(v.is_struct_value());

        let str_ptr = scope
            .builder
            .build_struct_gep(runtime_type_str, elem, 1, "load-tagget-union-val")
            .unwrap();
        scope.builder.build_store(str_ptr, v);
        let tag_ptr = scope
            .builder
            .build_struct_gep(runtime_type_str, elem, 0, "load-tagget-union-tag")
            .unwrap();
        scope
            .builder
            .build_store(tag_ptr, ty_u8.const_int(tag_v, false));

        println!("{}", engine.dump_ir());

        let array_nth = unsafe {
            scope.builder.build_gep(
                array_runtime_type,
                array,
                &[ty_u32.const_int(i as _, false)],
                "load-array-tagget-union-offset",
            )
        };

        let elem = scope
            .builder
            .build_load(runtime_type, elem, "load-tagged-union");
        scope.builder.build_store(array_nth, elem);
    }

    scope.builder.build_store(
        array,
        runtime_type_u64.const_named_struct(&[
            ty_u64.const_int(4, false).into(),
            ty_u64.const_int(closure_handle(func), false).into(),
        ]),
    );

    /* let array = scope
    .builder
    .build_ptr_to_int(array, ty_u64, "argv-ptr-to-int"); */

    let args = [
        ty_u64
            .const_int(array_runtime_type.len() as u64, false)
            .into(),
        array.into(),
    ];

    let __ion_callback_handler_ty = ctx.struct_type(&[], false).fn_type(
        &[
            ty_u64.into(),
            ty_u64.ptr_type(llvm::AddressSpace::default()).into(),
        ],
        false,
    );

    let __ion_callback_handler = ty_u64.const_int(__ion_callback_handler as usize as u64, false);
    let int_ty = get_ctx().ptr_sized_int_type(engine.ee().get_target_data(), None);
    let __ion_callback_handler = scope.builder.build_int_to_ptr(
        __ion_callback_handler,
        int_ty.ptr_type(llvm::AddressSpace::default()),
        "tmp-int-to-ptr",
    );

    scope.builder.build_indirect_call(
        __ion_callback_handler_ty,
        __ion_callback_handler,
        &args,
        "sdaw",
    );

    if ty == Type::None {
        None
    } else {
        /* let result = scope.builder.build_load(runtime_type, array, "load-result");
        let result = result.into_struct_value(); */

        let union_ptr = scope
            .builder
            .build_struct_gep(runtime_type, array, 1, "load-tagget-union-variant-offset")
            .expect("");
        let result = scope.builder.build_load(
            Engine::type_enum(&ty),
            union_ptr,
            "load-tagged-union-variant",
        );
        Some(result)
    }
}

fn closure_handle<C, F>(mut func: C) -> u64
where
    C: IonCallback<F> + 'static,
{
    let closure_trait: Box<Callback> = Box::new(move |args: &[RuntimeValue]| -> RuntimeValue {
        let func = &mut func;
        func.call(args)
    });
    let closure_trait_ptr = Box::into_raw(Box::new(closure_trait)) as *mut c_void;

    closure_trait_ptr as _

    // TODO: fix the memory leak
    // user_fns.push(s as *mut c_void);
}

extern "C" fn __ion_callback_handler(argc: u64, argv: *mut c_void) {
    tracing::trace!("__ion_callback_handler");

    let args = unsafe { from_raw_parts_mut(argv as *mut RuntimeValue, argc as _) };

    let closure_handle: u64 = AsIonType::from_runtime(args[0]).expect("BUG: invalid runtime");
    let closure_trait_ref = unsafe { &mut *(closure_handle as *mut &mut Callback) };

    tracing::trace!("__ion_callback_handler args: {args:?}");
    let res = closure_trait_ref(&args[1..]);
    tracing::trace!("__ion_callback_handler return: {res:?}");
    args[0] = res;
}
