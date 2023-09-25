use std::process::exit;

use ion::State;

//

fn main() {
    let src = r#"
        print(4);

/* fn add(lhs: i32, rhs: i32): i32 {
    let x = lhs + rhs;
    return x
}

// fn add<T>(lhs: T, rhs: T): T {
//     return lhs + rhs
// }

fn main(): none {
    let a = 4
    print(add(a, 5))
} */
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
