use std::process::exit;

use ion::{OptLevel, State};

//

fn main() {
    tracing_subscriber::fmt::init();

    let src = r#"
        print("test");
        // print(42);
    "#;

    // let lvl = OptLevel::High;
    // let lvl = OptLevel::Medium;
    // let lvl = OptLevel::Low;
    let lvl = OptLevel::None;
    let state = State::new().with_opt_level(lvl).with_inlining(false);

    state.add("print", |v: &str| println!("{v}"));
    // state.add("print", |v: i32| println!("{v}"));

    state.run(src).unwrap_or_else(|err| {
        eprintln!("{}", err.pretty_print(true, src, "<src>"));
        exit(1)
    });

    // println!("\n==[[ IR  ]]==\n{}\n==[[ END ]]==", state.dump_ir());
}
