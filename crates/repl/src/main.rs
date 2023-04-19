use std::process::exit;

use ion_core::prelude::*;

//

fn main() {
    let ion = Ion::new();

    let src = r#"
fn a() { return 4; };
c = a();
print(c);
"#;

    let module = ion.parse_str(src).unwrap_or_else(|err| {
        eprintln!("{}", err.pretty_print(true, src, "<src>"));
        exit(0)
    });

    println!("{}", ion.to_yaml(&module));

    ion.compile_ast(&module).unwrap_or_else(|err| {
        eprintln!("{}", err.pretty_print(true, src, "<src>"));
        exit(0)
    });
}
