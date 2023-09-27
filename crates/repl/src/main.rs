use std::process::exit;

use ion::{OptLevel, State};

//

fn main() {
    tracing_subscriber::fmt::init();

    let src = r#"
        print(add(inc(), inc())); // 1
        print(add(inc(), inc())); // 5
        print(add(inc(), inc())); // 9
        print(add(inc(), inc())); // 13
        print(add(inc(), inc())); // 17
        print(add(inc(), inc())); // 21
        print(sqr(12));           // 144
    "#;

    let lvl = OptLevel::High;
    // let lvl = OptLevel::Medium;
    // let lvl = OptLevel::Low;
    // let lvl = OptLevel::None;
    let state = State::new().with_opt_level(lvl);

    let mut i = 0;
    state.add("inc", move || {
        let n = i;
        i += 1;
        n
    });
    state.add("sqr", move |v: i32| v * v);
    state.add("print", move |v: i32| {
        println!("{v}");
    });
    state.add("add", move |l: i32, r: i32| l + r);

    state.run(src).unwrap_or_else(|err| {
        eprintln!("{}", err.pretty_print(true, src, "<src>"));
        exit(1)
    });

    // println!("\n==[[ IR  ]]==\n{}\n==[[ END ]]==", state.dump_ir());
}
