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
        if (true) {
            if (true) {
                print(42);
            };
        };
    "#;

    let lvl = OptLevel::High;
    // let lvl = OptLevel::Medium;
    // let lvl = OptLevel::Low;
    // let lvl = OptLevel::None;
    let state = State::new().with_opt_level(lvl);

    state.add("print", |v: i32| {
        println!("{v}");
    });

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
