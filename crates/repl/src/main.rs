use ion_core::{
    ast::{Interpret, Module},
    DebugTree, Ion,
};
use std::{collections::HashMap, io::stdout};

//

fn main() {
    let ion = Ion::new();

    let module = ion.parse_str(
        r#"
c = 0;
f = fn(a, b) {
    c = a + b;
};

f("ttttttttttttt
          tttttttt", 4334);
"#,
    );

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

    println!("{:#?}", module.debug_tree());

    let mut local = vec![HashMap::new()];
    println!("{:#?}", module.eval(&mut local));
    println!("{:#?}", local);

    // Module
    //   Assignment
    //     target: x
    //     value: FunctionCall
    //       name: "print"
    //       params: []
}
