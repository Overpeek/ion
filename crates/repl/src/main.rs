use std::process::exit;

use ion::State;

//

fn main() {
    let src = r#"
fn add(lhs: i32, rhs: i32): i32 {
    return lhs + rhs
}

/* fn add<T>(lhs: T, rhs: T): T {
    return lhs + rhs
} */

fn main(): none {
    let a = 4
    print(add(a, 5))
}
"#;

    let state = State::new();

    state.run(src).unwrap_or_else(|err| {
        eprintln!("{}", err.pretty_print(true, src, "<src>"));
        exit(0)
    });

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
