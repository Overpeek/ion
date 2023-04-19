use crate::{
    ast::{Assign, BinExpr, Block, Expr, Fn, Literal, Path, Return, ReturnVoid, Stmt},
    prelude::Module,
    util::ToStatic,
};
use ion_macros::ToStatic;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use thiserror::Error;

//

#[derive(Debug, Clone, Copy, ToStatic, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
#[to_static(result = "Self")]
pub enum IonType {
    Bool,
    Int,
    Float,
    Str,
    Void,

    // TODO:
    Struct,

    // TODO:
    Tuple,

    #[default]
    Unknown,
}

pub type TypeId = usize;

#[derive(Debug, Error, Clone, Copy)]
pub enum IonTypeError {
    #[error("Variable not assigned before it was read")]
    VariableNotAssigned,

    #[error("Variable type changed")]
    VariableTypeChanged,

    #[error("Return type changed")]
    ReturnTypeChanged,

    #[error("Type could not be resolved")]
    TypeNotResolved,
}

pub type IonTypeResult<T> = Result<T, IonTypeError>;

pub trait ResolveType {
    fn type_of(&mut self, vars: Vars) -> IonTypeResult<IonType>;

    fn type_of_assume_resolved(&mut self) -> IonType {
        self.type_of(&mut HashMap::new())
            .expect("type wasn't resolved")
    }
}

type Vars<'a> = &'a mut HashMap<String, Vec<IonType>>;

//

impl ResolveType for Module<'_> {
    fn type_of(&mut self, vars: Vars) -> IonTypeResult<IonType> {
        self.start.type_of(vars)
    }
}

impl ResolveType for Fn<'_> {
    fn type_of(&mut self, vars: Vars) -> IonTypeResult<IonType> {
        self.block.type_of(vars)
    }
}

impl ResolveType for Block<'_> {
    fn type_of(&mut self, vars: Vars) -> IonTypeResult<IonType> {
        if self.ty != IonType::Unknown {
            return Ok(self.ty);
        }

        for stmt in self.stmts.iter_mut() {
            let ty = stmt.type_of(vars)?;

            if matches!(stmt, Stmt::Return(..) | Stmt::ReturnVoid(..)) {
                if ty == IonType::Unknown {
                    return Err(IonTypeError::TypeNotResolved);
                }

                if self.ty != IonType::Unknown && self.ty != ty {
                    return Err(IonTypeError::ReturnTypeChanged);
                }
                self.ty = ty;
            }
        }

        Ok(self.ty)
    }
}

impl ResolveType for Stmt<'_> {
    fn type_of(&mut self, vars: Vars) -> IonTypeResult<IonType> {
        match self {
            Stmt::Assign(v) => v.type_of(vars),
            Stmt::FnCall(_) => todo!(),
            Stmt::Fn(_) => todo!(),
            Stmt::Return(ret) => ret.type_of(vars),
            Stmt::ReturnVoid(ret) => ret.type_of(vars),
            Stmt::Conditional() => todo!(),
            Stmt::IteratorLoop() => todo!(),
            Stmt::Loop() => todo!(),
        }
    }
}

impl ResolveType for Assign<'_> {
    fn type_of(&mut self, vars: Vars) -> IonTypeResult<IonType> {
        if self.ty != IonType::Unknown {
            return Ok(self.ty);
        }

        let ty = self.value.type_of(vars)?;

        if ty == IonType::Unknown {
            return Err(IonTypeError::TypeNotResolved);
        }
        self.ty = ty;

        vars.entry(self.target.to_string())
            .or_default()
            .push(self.ty);

        Ok(self.ty)
    }
}

impl ResolveType for Return<'_> {
    fn type_of(&mut self, vars: Vars) -> IonTypeResult<IonType> {
        self.value.type_of(vars)
    }
}

impl ResolveType for ReturnVoid {
    fn type_of(&mut self, _: Vars) -> IonTypeResult<IonType> {
        Ok(IonType::Void)
    }
}

impl ResolveType for Expr<'_> {
    fn type_of(&mut self, vars: Vars) -> IonTypeResult<IonType> {
        match self {
            Expr::NamelessFn(f) => todo!(),
            Expr::FnCall(_) => todo!(),
            Expr::UnExpr() => todo!(),
            Expr::BinExpr(v) => v.type_of(vars),
            Expr::Literal(v) => v.type_of(vars),
            Expr::Path(v) => v.type_of(vars),
        }
    }
}

impl ResolveType for BinExpr<'_> {
    fn type_of(&mut self, vars: Vars) -> IonTypeResult<IonType> {
        if self.ty != IonType::Unknown {
            return Ok(self.ty);
        }

        let lhs = self.sides.0.type_of(vars)?;
        let rhs = self.sides.1.type_of(vars)?;

        if lhs == IonType::Unknown || rhs == IonType::Unknown {
            return Err(IonTypeError::TypeNotResolved);
        }
        if lhs != rhs {
            todo!();
        }
        self.ty = lhs;

        Ok(lhs)
    }
}

impl ResolveType for Literal<'_> {
    fn type_of(&mut self, _: Vars) -> IonTypeResult<IonType> {
        Ok(match self {
            Literal::Bool(_) => IonType::Bool,
            Literal::Int(_) => IonType::Int,
            Literal::Float(_) => IonType::Float,
            Literal::String(_) => IonType::Str,
        })
    }
}

impl ResolveType for Path<'_> {
    fn type_of(&mut self, vars: Vars) -> IonTypeResult<IonType> {
        if self.ty != IonType::Unknown {
            return Ok(self.ty);
        }

        let part = self.parts.first().unwrap(); // TODO: whole path
        let Some(ty) = vars.get(part.as_ref()).and_then(|stack| stack.last()).copied() else {
            return Err(IonTypeError::VariableNotAssigned);
        };

        if ty == IonType::Unknown {
            return Err(IonTypeError::TypeNotResolved);
        }
        self.ty = ty;

        Ok(ty)
    }
}
