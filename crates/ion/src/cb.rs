use crate::{
    ty::Type,
    val::{AsIonType, RuntimeValue},
};

//

pub trait IonCallback<F> {
    const ARGS: &'static [Type];
    const TYPE: Type;

    fn args(&self) -> &'static [Type] {
        Self::ARGS
    }

    fn ty(&self) -> Type {
        Self::TYPE.clone()
    }

    fn call(&mut self, args: &[RuntimeValue]) -> RuntimeValue;
}

impl<F, R> IonCallback<(R,)> for F
where
    F: FnMut() -> R,
    R: AsIonType,
{
    const ARGS: &'static [Type] = &[];
    const TYPE: Type = R::TYPE;

    fn call(&mut self, args: &[RuntimeValue]) -> RuntimeValue {
        assert_eq!(args.len(), 0);
        AsIonType::to_runtime((self)())
    }
}

impl<F, A1, R> IonCallback<(A1, R)> for F
where
    F: FnMut(A1) -> R,
    A1: AsIonType,
    R: AsIonType,
{
    const ARGS: &'static [Type] = &[A1::TYPE];
    const TYPE: Type = R::TYPE;

    fn call(&mut self, args: &[RuntimeValue]) -> RuntimeValue {
        assert_eq!(args.len(), 1);

        let a1 = A1::from_runtime_unwrap(args[0]);

        AsIonType::to_runtime((self)(a1))
    }
}

impl<F, A1, A2, R> IonCallback<(A1, A2, R)> for F
where
    F: FnMut(A1, A2) -> R,
    A1: AsIonType,
    A2: AsIonType,
    R: AsIonType,
{
    const ARGS: &'static [Type] = &[A1::TYPE, A2::TYPE];
    const TYPE: Type = R::TYPE;

    fn call(&mut self, args: &[RuntimeValue]) -> RuntimeValue {
        assert_eq!(args.len(), 2);

        let a1 = A1::from_runtime_unwrap(args[0]);
        let a2 = A2::from_runtime_unwrap(args[1]);

        AsIonType::to_runtime((self)(a1, a2))
    }
}
