use std::rc::Rc;

use self::Unlambda::*;

pub type Unlambda = Rc<UnlambdaData>;

#[derive(Debug)]
pub enum UnlambdaData {
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
