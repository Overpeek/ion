use std::process::exit;

use ion_core::Ion;

#[test]
fn invalid_token() {
    let code = r#"x = \"#;
    let ion = Ion::new();
    let err = ion.parse_str(code).unwrap_err();

    insta::assert_display_snapshot!(err.pretty_print(false, code, "<code>"));
}

#[test]
fn unexpected_eof() {
    let code = r#"x ="#;
    let ion = Ion::new();
    let err = ion.parse_str(code).unwrap_err();

    insta::assert_display_snapshot!(err.pretty_print(false, code, "<code>"));
}

#[test]
fn unexpected_token_1() {
    let code = r#"x = ="#;
    let ion = Ion::new();
    let err = ion.parse_str(code).unwrap_err();

    insta::assert_display_snapshot!(err.pretty_print(false, code, "<code>"));
}

#[test]
fn unexpected_token_2() {
    let code = r#"""#;
    let ion = Ion::new();
    let err = ion.parse_str(code).unwrap_err();

    insta::assert_display_snapshot!(err.pretty_print(false, code, "<code>"));
}

#[test]
fn correct_code() {
    let code = r#"
        x = 4;
    fn y() { x = 2; };
    y();"#;

    let ion = Ion::new();
    ion.parse_str(code).unwrap_or_else(|err| {
        eprintln!("{}", err.pretty_print(true, code, "<code>"));
        exit(-1);
    });
}
