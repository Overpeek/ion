use std::process::exit;

use ion_core::prelude::*;

//

fn main() {
    let ion = Ion::new();

    let src = r#"
// a = fn() {
//     b = fn() { return 4 }
//     return b
// }
// x = 4 + 4
// return (a())() + x
a = fn() { return 4 + 4 }
x = 4 + 4
return a() + x
// a = fn(x) { return x + x }
// c = 5 + 1
// return c
"#;

    let mut module = ion.parse_str(src).unwrap_or_else(|err| {
        eprintln!("{}", err.pretty_print(true, src, "<src>"));
        exit(0)
    });

    println!("{}", ion.to_yaml(&module));

    let module = ion.compile_ast(&mut module).unwrap_or_else(|err| {
        eprintln!("{}", err.pretty_print(true, src, "<src>"));
        exit(0)
    });
}
