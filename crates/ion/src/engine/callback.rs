use std::{ffi::c_void, slice::from_raw_parts_mut};

use crate::{syntax::Type, AsIonType, IonCallback, RuntimeValue};

use super::{get_ctx, llvm, Engine, Scope};

//

pub type Callback = dyn FnMut(&[RuntimeValue]) -> RuntimeValue + 'static;

//

pub fn call_closure<C, F>(
    engine: &Engine,
    scope: &Scope,
    func: C,
) -> Option<llvm::BasicValueEnum<'static>>
where
    C: IonCallback<F> + 'static,
{
    let ty = func.ty();

    // TODO: implement real structs and arrays
    // this basically calls the __ion_callback_handler with correct arguments
    // (array length + array pointer)
    // (each array value is a struct { u8, union { u64, i32, f32, bool, struct {} } })
    let ctx = get_ctx();

    let runtime_type_u64 = ctx.i64_type();
    let runtime_type_tag = ctx.i8_type();
    let runtime_type = ctx.struct_type(&[runtime_type_tag.into(), runtime_type_u64.into()], false);

    let mut array_args: Vec<llvm::StructValue> = [runtime_type.const_named_struct(&[])]
        .into_iter()
        .chain(func.args().iter().enumerate().map(|(i, arg)| {
            let v = engine.load_variable(&format!("{i}"), scope);
            let tag = match arg {
                Type::U64 => 4,
                Type::I32 => 3,
                Type::F32 => 2,
                Type::Bool => 1,
                Type::None => 0,
            };

            runtime_type.const_named_struct(&[runtime_type_tag.const_int(tag, false).into(), v])
        }))
        .collect();
    array_args[0] = runtime_type.const_named_struct(&[
        runtime_type_tag.const_int(4, false).into(),
        runtime_type_u64
            .const_int(closure_handle(func), false)
            .into(),
    ]);

    tracing::trace!("{array_args:#?}");

    let array = engine.entry_var_alloca(
        "argv",
        runtime_type.array_type(array_args.len() as u32).into(),
        scope,
    );
    scope
        .builder
        .build_store(array, runtime_type.const_array(&array_args[..]));

    /* let array = scope
    .builder
    .build_ptr_to_int(array, runtime_type_u64, "argv-ptr-to-int"); */

    let args = [
        runtime_type_u64
            .const_int(array_args.len() as u64, false)
            .into(),
        array.into(),
    ];

    let __ion_callback_handler_ty = ctx.struct_type(&[], false).fn_type(
        &[
            runtime_type_u64.into(),
            runtime_type_u64
                .ptr_type(llvm::AddressSpace::default())
                .into(),
        ],
        false,
    );

    let __ion_callback_handler =
        runtime_type_u64.const_int(__ion_callback_handler as usize as u64, false);
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
    // println!("__ion_callback_handler");

    let args = unsafe { from_raw_parts_mut(argv as *mut RuntimeValue, argc as _) };

    let closure_handle: u64 = AsIonType::from_runtime(args[0]).expect("BUG: invalid runtime");
    let closure_trait_ref = unsafe { &mut *(closure_handle as *mut &mut Callback) };

    // println!("__ion_callback_handler args: {args:?}");
    let res = closure_trait_ref(&args[1..]);
    // println!("__ion_callback_handler return: {res:?}");
    args[0] = res;
}
