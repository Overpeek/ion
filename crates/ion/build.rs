use std::error::Error;

//

fn main() -> Result<(), Box<dyn Error>> {
    // println!("cargo:rustc-link-lib=dylib=ffi");

    lalrpop::Configuration::new()
        .emit_rerun_directives(true)
        .use_colors_if_tty()
        .process_current_dir()
}
