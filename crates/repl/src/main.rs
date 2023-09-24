use std::process::exit;

use ion::prelude::*;

//

fn main() {
    println!("ion new");
    let ion = Ion::new();

    let src = r#"
// a = fn() {
//     b = fn() { return 4 }
//     return b
// }
// x = 4 + 4
// return (a())() + x
return fn() { return fn() { return 4 } }()() + 4
// a = fn(x) { return x + x }
// c = 5 + 1
// return c
"#;

    println!("ion parse");
    let mut module = ion.parse_str(src).unwrap_or_else(|err| {
        eprintln!("{}", err.pretty_print(true, src, "<src>"));
        exit(0)
    });

    println!("ion to yaml");
    println!("{}", ion.to_yaml(&module));

    println!("ion compile");
    let module = ion.compile_ast(&mut module).unwrap_or_else(|err| {
        eprintln!("{}", err.pretty_print(true, src, "<src>"));
        exit(0)
    });
}
