use std::process::exit;

use ion::{OptLevel, State};

//

fn main() {
    let src = r#"
        let x = 4;
        print(x);
        x = 5;
        print(x);
        x += 5;
        print(x);
        x *= 5;
        print(x);

        let limit = value();
        if (x >= limit) {
            print(x);
        };
    "#;

    let lvl = OptLevel::High;
    // let lvl = OptLevel::Medium;
    // let lvl = OptLevel::Low;
    // let lvl = OptLevel::None;
    let state = State::new().with_opt_level(lvl);

    /* let print = |v: i32| {
        println!("{v}");
    }; */

    /* #[derive(Debug)]
    #[repr(C, u8)]
    enum RuntimeValue {
        I32(i32),
        F32(f32),
        Bool(bool),
        None,
    }

    extern "C" fn callback_handler(func: i32, argc: i32, argv: *mut std::ffi::c_void) {
        if argc < 0 {
            unreachable!()
        }

        let args =
            unsafe { std::slice::from_raw_parts_mut(argv as *mut RuntimeValue, argc as usize) };

        println!("callback_handler: {args:?}");
    } */

    /* let cb: Box<Box<dyn FnMut(i32)>> = Box::new(Box::new(print));

    extern "C" fn closure_handler() {}
    let fp = Box::into_raw(cb) as *mut std::ffi::c_void; */

    fn print(v: i32) {
        println!("{v}");
    }
    let print: fn(i32) = print;

    state.add("print", print);

    fn value() -> i32 {
        42
    }
    let value: fn() -> i32 = value;

    state.add("value", value);

    state.run(src).unwrap_or_else(|err| {
        eprintln!("{}", err.pretty_print(true, src, "<src>"));
        exit(1)
    });

    println!("\n==[[ IR  ]]==\n{}\n==[[ END ]]==", state.dump_ir());

    /* let mut module = ion.parse_str(src).unwrap_or_else(|err| {
        eprintln!("{}", err.pretty_print(true, src, "<src>"));
        exit(0)
    });

    println!("{}", ion.to_yaml(&module));

    let module = ion.compile_ast(&mut module).unwrap_or_else(|err| {
        eprintln!("{}", err.pretty_print(true, src, "<src>"));
        exit(0)
    }); */
}
