use inkwell::values::BasicValueEnum;

use crate::ast::Assign;

use super::{Compile, Compiler};

//

impl Compile for Assign<'_> {
    fn compile<'a>(&mut self, compiler: &mut Compiler<'a>) -> Option<BasicValueEnum<'a>> {
        let value = self.value.compile(compiler).expect("Expr returned nothing");
        let key = self.target.to_string();

        compiler.vars.entry(key).or_default().push(value);

        None
    }
}
