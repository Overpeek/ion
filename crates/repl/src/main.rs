use std::process::exit;

use ion::State;

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
        let c = true;
        if (c) {
            print(42);
        };
    "#;

    let state = State::new();

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
