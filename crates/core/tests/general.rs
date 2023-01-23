use ion_core::Module;

#[test]
fn simple_general() {
    let ion = Ion::new();

    let mut memory = Memory::default();

    let module: Module = ion
        .parse_str(
            r#"
return function(a, b) {
    return a + b;
};
"#,
        )
        .unwrap();

    match module.run(&mut memory) {
        IonItem::Function(f) => {
            assert_eq!(f.call((5, 10)), IonItem::I32(15));
        }
        _ => {
            unreachable!();
        }
    }
}
