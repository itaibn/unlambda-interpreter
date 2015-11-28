use std::ops::Deref;
use std::rc::Rc;

use self::UnlambdaEnum::*;

#[derive(Clone, Debug)]
pub struct Unlambda {inner: Rc<UnlambdaEnum>}

#[derive(Clone, Debug)]
pub enum UnlambdaEnum {
    Apply(Unlambda, Unlambda),
    K,
    K1(Unlambda),
    S,
    S1(Unlambda),
    S2(Unlambda, Unlambda),
    I,
    V,
    C,
    Cont(Continuation),
    D,
    D1(Unlambda),
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

/*
    pub fn inner(&self) -> &UnlambdaEnum {
        &*self.inner
    }
*/
}

macro_rules! mk_expr {
    ([$e:expr]) => ($e);
    ($t:ident) => (Unlambda::new($t));
    ($t:ident; $($e:tt),*) => (Unlambda::new($t($(mk_expr! ($e))),*));
    (($x:tt)) => mk_expr! $(x)
}

impl Deref for Unlambda {
    type Target = UnlambdaEnum;

    fn deref(&self) -> &UnlambdaEnum {
        self.inner.deref()
    }
}

#[derive(Clone, Debug)]
pub struct Continuation {inner: Rc<Option<ContStruct>>}

#[derive(Clone, Debug)]
struct ContStruct {
    part: ContPart,
    next: Continuation,
}

#[derive(Clone, Debug)]
enum ContPart {
    AppOn(Unlambda),
    ArgOf(Unlambda),
//    ArgOfUnevaled(Unlambda),
}

use self::ContPart::*;

impl Continuation {
    pub fn new() -> Continuation {
        Continuation {inner: Rc::new(None)}
    }

    fn add_part(&self, part: ContPart) -> Continuation {
        Continuation {
            inner: Rc::new(ContStruct {
                part: part,
                next: self.clone(),
            })
        }
    }

    fn eval(&self, expr: Unlambda) -> Result<Task, Unlambda> {
        match expr {
            Apply(func, arg) => Task::Eval(func, self.add_part(AppOn(arg))),
            _ => self.throw(expr),
        }
    }

    fn throw(&self, obj: Unlambda) -> Result<Task, Unlambda> {
        let stct = *self.unwrap_or_else(|| {return Err(obj);});
        Ok(
            match stct.part {
                AppOn(arg) => match obj {
                    D => Task::Eval(D1(obj), stct.next),
                    _ => Task::Eval(arg, stct.next.add_part(ArgOf(obj))),
                },
                ArgOf(func) => Task::Apply(func, obj, stct.next),
//                ArgOfUnevaled(expr) => Task::Eval(expr,
//                    stct.next.add_part(AppOn(
            }
        )
    }
}

pub enum Task {
    Eval(Unlambda, Continuation),
    Apply(Unlambda, Unlambda, Continuation),
}

impl Task {
    pub fn main_loop(self) -> Unlambda {
        let mut task = self;
        loop {
            task = task.single_step().unwrap_or_else(|unl| {return unl;})
        }
    }

    fn single_step(self) -> Result<Task, Unlambda> {
        match self {
            Task::Eval(expr, cont) => cont.eval(expr),
            Task::Apply(func, arg, cont) => func.apply(arg, cont),
        }
    }
}

impl Unlambda {
    fn apply(self, arg: Unlambda, cont: Continuation) -> Result<Task, Unlambda>
    {
        macro_rules! result {($($x:tt)*) => {cont.throw(mk_expr! ($($x)*))}}

        match *self.inner {
            K => cont.throw(mk_expr! (K1; [arg])),
            K1(c) => cont.throw(c),
            S => result! (S1; [arg]),
            S1(x) => result! (S2; [x], [arg]),
            S2(x, y) => result! (Apply(Apply([x], [arg]), Apply([y], [arg]))),
            I => cont.throw(arg),
            V => result! (V),
            C => result! (Apply([arg], Cont([cont.clone()]))),
            Cont(alt_cont) => alt_cont.throw(arg),
            D => panic!("Internal error: d applied on evaluated expression."),
            D1(expr) => cont.add_part(ContPart::ArgOn(arg)).eval(expr),
            E => Err(arg),
            Dot(_) | At | Query(_) | Pipe => panic!("IO not yet implemented"),
        }
    }
}
