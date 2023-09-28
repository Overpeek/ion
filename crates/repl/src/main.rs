use std::process::exit;

use ion::{OptLevel, State};
use rand::Rng;

//

fn main() {
    tracing_subscriber::fmt::init();

    let src = r#"
        if rand() {
            print(42);
        }

        // for i in 0..10 {}
    "#;

    let lvl = OptLevel::High;
    // let lvl = OptLevel::Medium;
    // let lvl = OptLevel::Low;
    // let lvl = OptLevel::None;
    let state = State::new().with_opt_level(lvl);

    let mut rng = rand::thread_rng();
    state.add("rand", move || rng.gen_ratio(1, 2));
    state.add("print", |v: i32| println!("{v}"));

    state.run(src).unwrap_or_else(|err| {
        eprintln!("{}", err.pretty_print(true, src, "<src>"));
        exit(1)
    });

    // println!("\n==[[ IR  ]]==\n{}\n==[[ END ]]==", state.dump_ir());
}
