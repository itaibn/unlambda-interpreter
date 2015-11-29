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
    fn is_d(&self) -> bool {
        match *self.inner {
            D => true,
            _ => false,
        }
    }
}

macro_rules! mk_expr {
    ([$e:expr]) => ($e);
    (($($x:tt)*)) => (mk_expr! ($($x)*));
    ($t:ident) => (Unlambda::new($t));
    ($t:ident: $($e:tt),*) => (Unlambda::new($t($(mk_expr! ($e)),*)));
}

impl Deref for Unlambda {
    type Target = UnlambdaEnum;

    fn deref(&self) -> &UnlambdaEnum {
        self.inner.deref()
    }
}

#[derive(Clone, Debug)]
pub struct Continuation {inner: Option<Rc<ContStruct>>}

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
        Continuation {inner: None}
    }

    fn add_part(&self, part: ContPart) -> Continuation {
        Continuation {
            inner: Some(Rc::new(ContStruct {
                part: part,
                next: self.clone(),
            }))
        }
    }

    fn eval(&self, expr: Unlambda) -> Result<Task, Unlambda> {
        if let Apply(ref func, ref arg) = *expr.inner {
            return Ok(Task::Eval(func.clone(),
                                 self.add_part(AppOn(arg.clone()))));
        }
        self.throw(expr)
    }

    fn throw(&self, obj: Unlambda) -> Result<Task, Unlambda> {
        match self.inner {
            None => {return Err(obj);},
            Some(ref stct) => Ok(
                match stct.part {
                    AppOn(ref arg) => if obj.is_d() {
                        Task::Eval(Unlambda::new(D1(obj)), stct.next.clone())
                    } else {
                        Task::Eval(arg.clone(), stct.next.add_part(ArgOf(obj)))
                    },
                    ArgOf(ref func) => Task::Apply(func.clone(), obj,
                        stct.next.clone()),
//                ArgOfUnevaled(expr) => Task::Eval(expr,
//                    stct.next.add_part(AppOn(
                }
            )
        }
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
            match task.single_step() {
                Ok(more) => {task = more;},
                Err(result) => {return result;},
            }
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
            Apply(_, _) => unreachable!(),
            K => cont.throw(mk_expr! (K1: [arg])),
            K1(ref c) => cont.throw(c.clone()),
            S => result! (S1: [arg]),
            S1(ref x) => result! (S2: [x.clone()], [arg]),
            S2(ref x, ref y) => result! (Apply: (Apply: [x.clone()],
                [arg.clone()]), (Apply: [y.clone()], [arg])),
            I => cont.throw(arg),
            V => result! (V),
            C => result! (Apply: [arg], (Cont: [cont.clone()])),
            Cont(ref alt_cont) => alt_cont.clone().throw(arg),
            D => panic!("Internal error: d applied on evaluated expression."),
            D1(ref expr) =>
                cont.add_part(ContPart::AppOn(arg)).eval(expr.clone()),
            E => Err(arg),
            Dot(_) | At | Query(_) | Pipe => panic!("IO not yet implemented"),
            //_ => unimplemented!(),
        }
    }

    pub fn eval(self) -> Unlambda {
        Continuation::new().eval(self).map(|t| t.main_loop()).unwrap_or_else(|u|
            u)
    }
}
