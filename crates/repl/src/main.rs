use ion_core::Ion;
use std::process::exit;

//

fn main() {
    let ion = Ion::new();

    let src = r#"
c = 0;
f = fn(a, b) {
    c = a + b;"44444444
        4444"
"#; /* };

    f\("ttttttttttttt
              tttttttt", 4334);
    "#; */

    let module = ion.parse_str(src).unwrap_or_else(|err| {
        err.pretty_print(true, src, "<src>");
        exit(0)
    });

    println!("{}", ion.to_yaml(&module));

    ion.compile_ast(&module).unwrap_or_else(|err| {
        err.pretty_print(true, src, "<src>");
        exit(0)
    });

    /*
    f = function() {
        print(4);
    };

    f();

    function f() {
        print(5);
    }

    f();

    x = 4;
         * */

    // Module
    //   Assignment
    //     target: x
    //     value: FunctionCall
    //       name: "print"
    //       params: []
}
