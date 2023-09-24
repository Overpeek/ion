use inkwell::values::BasicValueEnum;

use super::{Compile, Compiler};
use crate::ast::Path;

//

impl Compile for Path<'_> {
    fn compile<'a>(&mut self, compiler: &mut Compiler<'a>) -> Option<BasicValueEnum<'a>> {
        let part = self.parts.first().unwrap(); // TODO: whole path
        let value = *compiler.vars.get(part.as_ref()).unwrap().last().unwrap();
        Some(value)
    }
}
