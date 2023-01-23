use ion_core::{ast::Module, DebugTree, Ion};
use std::io::stdout;

//

fn main() {
    let ion = Ion::new();

    let module = ion.parse_str(
        r#"
f = function() {
    print(4);
};

f();

function f() {
    print(5);
}

f();

x = 4;
"#,
    );

    println!("{:#?}", module.debug_tree());

    // Module
    //   Assignment
    //     target: x
    //     value: FunctionCall
    //       name: "print"
    //       params: []
}
