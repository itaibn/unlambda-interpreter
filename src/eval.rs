use std::rc::Rc;

#[derive(Debug)]
pub struct Unlambda {inner: Rc<UnlambdaEnum>}

#[derive(Debug)]
pub enum UnlambdaEnum {
    Apply(Unlambda, Unlambda),
    K,
    S,
    I,
    V,
    C,
    D,
    Dot(char),
    E,
    At,
    Query(char),
    Pipe,
}

impl Unlambda {
    pub fn new(data: UnlambdaEnum) -> Unlambda {
        Unlambda {inner: Rc::new(data)}
    }
}
