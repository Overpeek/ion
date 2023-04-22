use std::{
    collections::{
        hash_map::{DefaultHasher, Entry},
        HashMap,
    },
    hash::{Hash, Hasher},
    sync::atomic::{AtomicU32, Ordering},
};

use ion_macros::ToStatic;
use serde::{Deserialize, Serialize};
use thiserror::Error;

use crate::{
    ast::{Assign, BinExpr, Block, Expr, Fn, FnCall, Literal, Path, Return, ReturnVoid, Stmt},
    prelude::Module,
    util::ToStatic,
};

//

#[derive(Debug, Clone, Copy, ToStatic, PartialEq, Eq, Hash, Default, Serialize, Deserialize)]
#[to_static(result = "Self")]
pub enum IonType {
    Bool,
    Int,
    Float,
    Str,
    Void,

    NamelessFn {
        id: u32,
    },

    // TODO:
    Struct,

    // TODO:
    Tuple,

    #[default]
    Unknown,
}

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
    fn type_of(&mut self, res: &mut TypeResolver) -> IonTypeResult<IonType>;

    fn type_of_resolved(&self) -> IonTypeResult<IonType>;
}

#[derive(Debug, Default, Serialize)]
pub struct TypeResolver {
    vars: HashMap<String, Vec<IonType>>,
    gen_fns: HashMap<GenericFnIdentifier, Fn<'static>>,
    fns: HashMap<FnIdentifier, Fn<'static>>,
}

#[derive(Debug, PartialEq, Eq, Hash, Serialize)]
struct GenericFnIdentifier {
    fn_id: u32,
}

#[derive(Debug, PartialEq, Eq, Hash, Serialize)]
struct FnIdentifier {
    fn_id: u32,
    params: Vec<IonType>,
}

/* fn hash(i: &impl Hash) -> usize {
    let mut hasher = DefaultHasher::new();
    i.hash(&mut hasher);
    hasher.finish() as usize
} */

// #[derive(Debug, Default, Serialize)]
// pub struct TypeId(AtomicU32);

// impl ToStatic for TypeId {
//     type Static = Self;
//
//     fn to_static(&self) -> Self::Static {
//         self.clone()
//     }
// }
//
// impl Clone for TypeId {
//     fn clone(&self) -> Self {
//         Self(self.0.load(Ordering::SeqCst).into())
//     }
// }

/* impl TypeId {
    pub fn get(&self) -> u32 {
        self.
    }
} */

// type Vars<'a> = &'a mut HashMap<String, Vec<IonType>>;

// #[derive(PartialEq, Eq)]
/* enum Var {
    NamelessFn { id: usize, f: Fn<'static> },
    Normal(IonType),
} */

/* impl PartialEq for Var {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::NamelessFn { id: l_id, .. }, Self::NamelessFn { id: r_id, .. }) => l_id == r_id,
            (Self::Normal(l0), Self::Normal(r0)) => l0 == r0,
            _ => false,
        }
    }
} */

//

/* struct TyModule<'a> {
    pub ty: IonType,
    module: &'a Module<'a>,
} */

fn idgen() -> u32 {
    static IDGEN: AtomicU32 = AtomicU32::new(1);
    IDGEN.fetch_add(1, Ordering::SeqCst)
}

impl ResolveType for Module<'_> {
    fn type_of(&mut self, res: &mut TypeResolver) -> IonTypeResult<IonType> {
        self.start.type_of(res)?;
        Ok(IonType::Unknown)
    }

    fn type_of_resolved(&self) -> IonTypeResult<IonType> {
        Ok(IonType::Unknown)
    }
}

impl ResolveType for Fn<'_> {
    fn type_of(&mut self, res: &mut TypeResolver) -> IonTypeResult<IonType> {
        if self.ty != IonType::Unknown {
            return Ok(self.ty);
        }

        let id = idgen();
        self.ty = IonType::NamelessFn { id };

        if self.params.is_empty() {
            self.block.type_of(res)?;
            res.fns.insert(
                FnIdentifier {
                    fn_id: id,
                    params: vec![],
                },
                self.to_static(),
            );
        } else {
            res.gen_fns
                .insert(GenericFnIdentifier { fn_id: id }, self.to_static());
            // generic function
        }

        Ok(self.ty)
    }

    fn type_of_resolved(&self) -> IonTypeResult<IonType> {
        (self.ty != IonType::Unknown)
            .then_some(self.ty)
            .ok_or(IonTypeError::TypeNotResolved)
    }
}

impl ResolveType for Block<'_> {
    fn type_of(&mut self, res: &mut TypeResolver) -> IonTypeResult<IonType> {
        if self.ty != IonType::Unknown {
            return Ok(self.ty);
        }

        for stmt in self.stmts.iter_mut() {
            let ty = stmt.type_of(res)?;

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

    fn type_of_resolved(&self) -> IonTypeResult<IonType> {
        (self.ty != IonType::Unknown)
            .then_some(self.ty)
            .ok_or(IonTypeError::TypeNotResolved)
    }
}

impl ResolveType for Stmt<'_> {
    fn type_of(&mut self, res: &mut TypeResolver) -> IonTypeResult<IonType> {
        match self {
            Stmt::Assign(v) => v.type_of(res),
            Stmt::Expr(v) => v.type_of(res),
            // Stmt::FnCall(_) => todo!(),
            // Stmt::Fn(_) => todo!(),
            Stmt::Return(v) => v.type_of(res),
            Stmt::ReturnVoid(v) => v.type_of(res),
            Stmt::Conditional() => todo!(),
            Stmt::IteratorLoop() => todo!(),
            Stmt::Loop() => todo!(),
        }
    }

    fn type_of_resolved(&self) -> IonTypeResult<IonType> {
        match self {
            Stmt::Assign(v) => v.type_of_resolved(),
            Stmt::Expr(v) => v.type_of_resolved(),
            Stmt::Return(v) => v.type_of_resolved(),
            Stmt::ReturnVoid(v) => v.type_of_resolved(),
            Stmt::Conditional() => todo!(),
            Stmt::IteratorLoop() => todo!(),
            Stmt::Loop() => todo!(),
        }
    }
}

impl ResolveType for FnCall<'_> {
    fn type_of(&mut self, res: &mut TypeResolver) -> IonTypeResult<IonType> {
        let fn_ty = res
            .vars
            .get(self.name.as_ref())
            .and_then(|stack| stack.last())
            .ok_or(IonTypeError::VariableNotAssigned)?;

        let ty = match fn_ty {
            IonType::NamelessFn { id } => {
                let id = *id;
                let params = self
                    .args
                    .iter_mut()
                    .map(|arg| arg.type_of(res))
                    .collect::<IonTypeResult<Vec<IonType>>>()?;
                let fn_id = FnIdentifier { fn_id: id, params };

                if let Some(f) = res.fns.get(&fn_id) {
                    f.block.type_of_resolved()
                } else {
                    let mut generated_fn = res
                        .gen_fns
                        .get(&GenericFnIdentifier { fn_id: id })
                        .unwrap()
                        .to_static();
                    generated_fn.type_of(res)?;
                    let ty = generated_fn.block.type_of_resolved();
                    res.fns.insert(fn_id, generated_fn);
                    ty
                }?
            }
            other => panic!("{other:?} is not callable"),
        };

        if ty == IonType::Unknown {
            return Err(IonTypeError::TypeNotResolved);
        }
        self.ty = ty;

        Ok(self.ty)
    }

    fn type_of_resolved(&self) -> IonTypeResult<IonType> {
        (self.ty != IonType::Unknown)
            .then_some(self.ty)
            .ok_or(IonTypeError::TypeNotResolved)
    }
}

impl ResolveType for Assign<'_> {
    fn type_of(&mut self, res: &mut TypeResolver) -> IonTypeResult<IonType> {
        if self.ty != IonType::Unknown {
            return Ok(self.ty);
        }

        let ty = self.value.type_of(res)?;

        if ty == IonType::Unknown {
            return Err(IonTypeError::TypeNotResolved);
        }
        self.ty = ty;

        res.vars
            .entry(self.target.to_string())
            .or_default()
            .push(self.ty);

        Ok(self.ty)
    }

    fn type_of_resolved(&self) -> IonTypeResult<IonType> {
        (self.ty != IonType::Unknown)
            .then_some(self.ty)
            .ok_or(IonTypeError::TypeNotResolved)
    }
}

impl ResolveType for Return<'_> {
    fn type_of(&mut self, res: &mut TypeResolver) -> IonTypeResult<IonType> {
        self.value.type_of(res)
    }

    fn type_of_resolved(&self) -> IonTypeResult<IonType> {
        self.value.type_of_resolved()
    }
}

impl ResolveType for ReturnVoid {
    fn type_of(&mut self, _: &mut TypeResolver) -> IonTypeResult<IonType> {
        self.type_of_resolved()
    }

    fn type_of_resolved(&self) -> IonTypeResult<IonType> {
        Ok(IonType::Void)
    }
}

impl ResolveType for Expr<'_> {
    fn type_of(&mut self, res: &mut TypeResolver) -> IonTypeResult<IonType> {
        match self {
            Expr::NamelessFn(v) => {
                // TODO: captures
                v.type_of(res)
            }
            Expr::FnCall(v) => v.type_of(res),
            Expr::UnExpr() => todo!(),
            Expr::BinExpr(v) => v.type_of(res),
            Expr::Literal(v) => v.type_of(res),
            Expr::Path(v) => v.type_of(res),
        }
    }

    fn type_of_resolved(&self) -> IonTypeResult<IonType> {
        match self {
            Expr::NamelessFn(v) => v.type_of_resolved(),
            Expr::FnCall(v) => v.type_of_resolved(),
            Expr::UnExpr() => todo!(),
            Expr::BinExpr(v) => v.type_of_resolved(),
            Expr::Literal(v) => v.type_of_resolved(),
            Expr::Path(v) => v.type_of_resolved(),
        }
    }
}

impl ResolveType for BinExpr<'_> {
    fn type_of(&mut self, res: &mut TypeResolver) -> IonTypeResult<IonType> {
        if self.ty != IonType::Unknown {
            return Ok(self.ty);
        }

        let lhs = self.sides.0.type_of(res)?;
        let rhs = self.sides.1.type_of(res)?;

        if lhs == IonType::Unknown || rhs == IonType::Unknown {
            return Err(IonTypeError::TypeNotResolved);
        }
        if lhs != rhs {
            todo!();
        }
        self.ty = lhs;

        Ok(lhs)
    }

    fn type_of_resolved(&self) -> IonTypeResult<IonType> {
        (self.ty != IonType::Unknown)
            .then_some(self.ty)
            .ok_or(IonTypeError::TypeNotResolved)
    }
}

impl ResolveType for Literal<'_> {
    fn type_of(&mut self, _: &mut TypeResolver) -> IonTypeResult<IonType> {
        self.type_of_resolved()
    }

    fn type_of_resolved(&self) -> IonTypeResult<IonType> {
        Ok(match self {
            Literal::Bool(_) => IonType::Bool,
            Literal::Int(_) => IonType::Int,
            Literal::Float(_) => IonType::Float,
            Literal::String(_) => IonType::Str,
        })
    }
}

impl ResolveType for Path<'_> {
    fn type_of(&mut self, res: &mut TypeResolver) -> IonTypeResult<IonType> {
        if self.ty != IonType::Unknown {
            return Ok(self.ty);
        }

        let part = self.parts.first().unwrap(); // TODO: whole path
        let Some(ty) = res.vars.get(part.as_ref()).and_then(|stack| stack.last()).copied() else {
            return Err(IonTypeError::VariableNotAssigned);
        };

        if ty == IonType::Unknown {
            return Err(IonTypeError::TypeNotResolved);
        }
        self.ty = ty;

        Ok(ty)
    }

    fn type_of_resolved(&self) -> IonTypeResult<IonType> {
        (self.ty != IonType::Unknown)
            .then_some(self.ty)
            .ok_or(IonTypeError::TypeNotResolved)
    }
}
