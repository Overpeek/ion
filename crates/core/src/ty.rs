use crate::ast;
use once_cell::sync::OnceCell;
use std::{
    collections::{hash_map::DefaultHasher, HashMap},
    hash::{Hash, Hasher},
};
use thiserror::Error;

//

// pub struct

#[derive(Debug)]
pub struct Module<'a> {
    // function templates
    functions: HashMap<&'a str, FnType<'a>>,
    // generated functions with concrete types
    concrete_functions: HashMap<String, FnType<'a>>,
    // fn currently being processed
    current: FnType<'a>,
    // typeid -> type resolver
    types: Vec<Option<IonType>>,
    // type -> typeid resolver
    typeids: HashMap<IonType, usize>,
}

#[derive(Clone)]
pub struct FnType<'a> {
    // name of the generated function with concrete types
    concrete_name: OnceCell<String>,
    // return count of zero -> IonType::None
    ret_count: usize,

    // function AST
    pub f: &'a ast::Fn<'a>,
    // variable -> typeid
    pub vars: HashMap<&'a str, TypeId>,
    // params: Vec<IonType>,
    // return typeid
    pub ret: TypeId,
}

impl<'a> std::fmt::Debug for FnType<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FnType")
            .field("concrete_name", &self.concrete_name)
            // .field("f", &self.f)
            .field("vars", &self.vars)
            .field("ret", &self.ret)
            .field("ret_count", &self.ret_count)
            .finish()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IonType {
    Bool,
    I32,
    F32,
    Str,

    Fn { params: Vec<TypeId>, ret: TypeId },

    None,
    // #[default]
    // Unknown,
}

pub type TypeId = usize;

/* #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub enum IonResolverType {
    Known(u64),

    #[default]
    Unknown,
} */

#[derive(Debug, Error, Clone, Copy)]
pub enum IonTypeError {
    #[error("Variable not assigned before it was read")]
    VariableNotAssigned,

    #[error("Variable type changed")]
    VariableTypeChanged,

    #[error("Return type changed")]
    ReturnTypeChanged,
}

pub type IonTypeResult<T> = Result<T, IonTypeError>;

/* trait TTest<'a> {
    fn ttest(&'a self, f: &mut FnType<'a>) -> IonType;
} */

//

impl<'a> FnType<'a> {
    fn new(f: &'a ast::Fn<'a>) -> Self {
        Self {
            concrete_name: <_>::default(),
            f,
            vars: <_>::default(),
            ret: <_>::default(),
            ret_count: <_>::default(),
        }
    }

    fn params(&self, types: &[Option<IonType>]) -> Option<Vec<IonType>> {
        self.f
            .params
            .iter()
            .map(|param| types[*self.vars.get(param.as_ref()).unwrap()])
            .collect()
    }

    fn fn_name(&self, types: &[Option<IonType>]) -> Option<&str> {
        self.concrete_name
            .get_or_try_init(|| {
                if types[self.ret] == None || self.vars.values().any(|ty| types[*ty] == None) {
                    return Err(());
                }

                let params = self.params(types).unwrap();
                Module::fn_name(&self.f.name, &params).ok_or(())
            })
            .ok()
            .map(String::as_str)
    }

    fn is_concrete(&self, types: &[Option<IonType>]) -> bool {
        self.fn_name(types).is_some()
    }
}

impl<'a> Module<'a> {
    pub fn new(ast: &'a ast::Module<'a>) -> IonTypeResult<Self> {
        let mut res = Self {
            functions: <_>::default(),
            concrete_functions: <_>::default(),
            current: FnType::new(&ast.start),

            types: <_>::default(),
            typeids: <_>::default(),
        };

        res.module(ast)?;

        while res.concreteify_run()? {}

        Ok(res)
    }

    pub fn concrete_functions(&self) -> impl Iterator<Item = (&str, &FnType)> {
        self.concrete_functions
            .iter()
            .map(|(name, f)| (name.as_str(), f))
    }

    fn type_of(&mut self, ty: TypeId) -> Option<IonType> {
        *self.types.get(ty)
    }

    fn typeid(&mut self, ty: IonType) -> TypeId {
        *self.typeids.entry(ty).or_insert_with(|| {
            let typeid = self.types.len();
            self.types.push(Some(ty));
            typeid
        })
    }

    fn new_unknown_ty(&mut self) -> TypeId {
        let typeid = self.types.len();
        self.types.push(None);
        typeid
    }

    fn fn_name(base: &str, params: &[IonType]) -> Option<String> {
        if params.iter().any(|t| *t == IonType::Unknown) {
            return None;
        }

        let ty_hash = hash(params);
        Some(format!("{base} {ty_hash}"))
    }

    fn concreteify_run(&mut self) -> IonTypeResult<bool> {
        let mut should_continue = false;

        for f in self.functions.keys().copied().collect::<Vec<_>>() {
            should_continue |= self.concreteify_fn(f)?;
        }

        Ok(should_continue)
    }

    fn concreteify_fn(&mut self, name: &'a str) -> IonTypeResult<bool> {
        let mut should_continue = false;
        let current = self.functions.get(name).unwrap();

        if current
            .fn_name(&self.types)
            .map(|name| self.concrete_functions.contains_key(name))
            .unwrap_or(false)
        {
            return Ok(should_continue);
        }

        self.current = current.clone();
        let ast = self.current.f;

        for arg in ast.params.iter() {
            self.current.vars.entry(arg.as_ref()).or_default();
        }

        for ast in ast.block.0 .0.iter() {
            self.stmt(ast)?;
        }

        if self.current.ret_count == 0 {
            self.current.ret = self.typeid(IonType::None);
        }

        if let Some(concrete_name) = self.current.fn_name(&self.types) {
            should_continue = true;
            self.concrete_functions
                .insert(concrete_name.to_string(), self.current.clone());
        }

        self.functions
            .insert(ast.name.as_ref(), self.current.clone());

        Ok(should_continue)
    }

    fn module(&mut self, ast: &'a ast::Module<'a>) -> IonTypeResult<()> {
        self._fn(&ast.start)?;
        Ok(())
    }

    fn _fn(&mut self, ast: &'a ast::Fn<'a>) -> IonTypeResult<IonType> {
        self.functions
            .entry(ast.name.as_ref())
            .or_insert_with(|| FnType::new(ast));

        Ok(IonType::Fn {
            params: ast.params.iter().map(|_| self.new_unknown_ty()).collect(),
            ret: self.new_unknown_ty(),
        })
    }

    fn stmt(&mut self, ast: &'a ast::Stmt<'a>) -> IonTypeResult<()> {
        match ast {
            ast::Stmt::Assign(ast) => self.assign(ast),
            ast::Stmt::FnCall(ast) => {
                _ = self.fn_call(ast)?; // return value discarded
                Ok(())
            }
            ast::Stmt::Fn(ast) => {
                _ = self._fn(ast)?;
                Ok(())
            }
            ast::Stmt::Return(ast) => self.ret(ast),
            ast::Stmt::ReturnVoid(ast) => self.ret_void(ast),
            ast::Stmt::Conditional() => todo!(),
            ast::Stmt::IteratorLoop() => todo!(),
            ast::Stmt::Loop() => todo!(),
        }
    }

    fn assign(&mut self, ast: &'a ast::Assign<'a>) -> IonTypeResult<()> {
        let ty = self.expr(&ast.value)?;
        if let Some(old_ty) = self.current.vars.insert(&ast.target, ty) {
            if old_ty != ty {
                return Err(IonTypeError::VariableTypeChanged);
            }
        }

        Ok(())
    }

    fn fn_call(&mut self, ast: &'a ast::FnCall<'a>) -> IonTypeResult<IonType<'a>> {
        let mut params = vec![];
        for ast in ast.args.iter() {
            params.push(self.expr(ast)?);
        }

        if let Some(f) =
            Self::fn_name(&ast.name, &params).and_then(|name| self.concrete_functions.get(&name))
        {
            return Ok(f.ret);
        }

        if let Some(f) = self.functions.get(&ast.name.as_ref()) {
            return Ok(f.ret);
        }

        Ok(IonType::Unknown)
    }

    fn ret(&mut self, ast: &'a ast::Return<'a>) -> IonTypeResult<()> {
        let ty = self.expr(&ast.value)?;
        self.ret_ty(ty)
    }

    fn ret_void(&mut self, _: &'a ast::ReturnVoid) -> IonTypeResult<()> {
        self.ret_ty(IonType::None)
    }

    fn ret_ty(&mut self, ty: IonType<'a>) -> IonTypeResult<()> {
        self.current.ret_count += 1;

        if self.current.ret == IonType::Unknown {
            self.current.ret = ty;
            return Ok(());
        }

        if self.current.ret != ty {
            return Err(IonTypeError::ReturnTypeChanged);
        }

        Ok(())
    }

    fn expr(&mut self, ast: &'a ast::Expr<'a>) -> IonTypeResult<IonType<'a>> {
        match ast {
            ast::Expr::NamelessFn(ast) => self._fn(ast),
            ast::Expr::FnCall(ast) => self.fn_call(ast),
            ast::Expr::UnExpr() => todo!(),
            ast::Expr::BinExpr(ast) => self.bin_expr(ast),
            ast::Expr::Literal(ast) => Ok(self.literal(ast)),
            ast::Expr::Path(ast) => self.path(ast),
        }
    }

    /* fn un_expr(&mut self, ast: &ast::Un) -> IonTypeResult<IonType> {
        ast;
    } */

    fn bin_expr(&mut self, ast: &'a ast::BinExpr<'a>) -> IonTypeResult<IonType<'a>> {
        let (lhs, rhs) = (&ast.sides.0, &ast.sides.1);
        let (lhs, rhs) = (self.expr(lhs)?, self.expr(rhs)?);

        // TODO: operators

        match (lhs, rhs) {
            (IonType::Bool, IonType::Bool) => Ok(IonType::Bool),
            (IonType::I32, IonType::I32) => Ok(IonType::I32),
            (IonType::F32, IonType::F32) => Ok(IonType::F32),
            (_, IonType::Unknown) => Ok(IonType::Unknown),
            (IonType::Unknown, _) => Ok(IonType::Unknown),
            (lhs, rhs) => todo!("operator not implemented for {lhs:?} and {rhs:?}"),
        }
    }

    fn literal(&self, ast: &'a ast::Literal<'a>) -> IonType<'a> {
        match ast {
            ast::Literal::Bool(_) => IonType::Bool,
            ast::Literal::Int(_) => IonType::I32,
            ast::Literal::Float(_) => IonType::F32,
            ast::Literal::String(_) => IonType::Str,
        }
    }

    fn path(&self, ast: &'a ast::Path<'a>) -> IonTypeResult<IonType<'a>> {
        let var = ast.parts.first().unwrap(); // TODO: structs
        self.current
            .vars
            .get(var.as_ref())
            .ok_or(IonTypeError::VariableNotAssigned)
            .copied()
    }
}

//

fn hash(s: impl Hash) -> u64 {
    let mut hasher = DefaultHasher::new();
    s.hash(&mut hasher);
    hasher.finish()
}
