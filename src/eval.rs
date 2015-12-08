use std::io;
use std::ops::Deref;
use std::rc::Rc;

//#[cfg(not(test))]
//use parse;
/*
#[cfg(test)]
mod parser {
    use std::io;
    fn read_one_char<B: io::Read>(b: B) -> io::Result<char> {
        unimplemented!()
    }
}
*/

use self::UnlambdaEnum::*;
use self::ContPart::*;

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
                        Task::Eval(Unlambda::new(D1(arg.clone())),
                            stct.next.clone())
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

#[derive(Debug)]
enum Task {
    Eval(Unlambda, Continuation),
    Apply(Unlambda, Unlambda, Continuation),
}

struct UnlambdaState<I:io::Read, O:io::Write> {
    input: I,
    output: O,
    cur_char: Option<char>,
    //task: Task,
}

impl Task {
    pub fn main_loop<I:io::Read, O:io::Write>(self, state: &mut UnlambdaState<I,
            O>) -> Unlambda {
        let mut task = self;
        loop {
            // Print step for debuggin
            //println!("{:?}", task);
            match task.single_step(state) {
                Ok(more) => {task = more;},
                Err(result) => {return result;},
            }
        }
    }

    fn single_step<I:io::Read, O:io::Write>(self, state: &mut UnlambdaState<I,
            O>) -> Result<Task, Unlambda> {
        match self {
            Task::Eval(expr, cont) => cont.eval(expr),
            Task::Apply(func, arg, cont) => func.apply(arg, cont, state),
        }
    }
}

impl Unlambda {
    fn apply<I:io::Read, O:io::Write>(self, arg: Unlambda, cont: Continuation,
            state: &mut UnlambdaState<I,O>) -> Result<Task, Unlambda> {
        use parse;

        match *self.inner {
            Apply(_, _) => unreachable!(),
            K => cont.throw(Unlambda::new(K1(arg))),
            K1(ref c) => cont.throw(c.clone()),
            S => cont.throw(Unlambda::new(S1(arg))),
            S1(ref x) => cont.throw(Unlambda::new(S2(x.clone(), arg))),
            S2(ref x, ref y) => Ok(Task::Apply(x.clone(), arg.clone(),
                cont.add_part(AppOn(Unlambda::new(Apply(y.clone(),
                        arg.clone())))))),
            I => cont.throw(arg),
            V => cont.throw(Unlambda::new(V)),
            C => Ok(Task::Apply(arg, Unlambda::new(Cont(cont.clone())), cont)),
            Cont(ref alt_cont) => alt_cont.clone().throw(arg),
            Dot(c) => {
                write!(state.output, "{}", c);
                cont.throw(arg)
            }
            D => panic!("Internal error: d applied on evaluated expression."),
            D1(ref expr) =>
                cont.add_part(ContPart::AppOn(arg)).eval(expr.clone()),
            E => Err(arg),
            At =>
                match parse::read_one_char(&mut state.input) {
                    Ok(c) => {
                        state.cur_char = Some(c);
                        Ok(Task::Apply(arg, Unlambda::new(I), cont))
                    },
                    Err(_) => {
                        state.cur_char = None;
                        Ok(Task::Apply(arg, Unlambda::new(V), cont))
                    },
            },
            Query(c) => {
                if state.cur_char == Some(c) {
                    Ok(Task::Apply(arg, Unlambda::new(I), cont))
                } else {
                    Ok(Task::Apply(arg, Unlambda::new(V), cont))
                }
            },
            Pipe => Ok(Task::Apply(arg,
                                   Unlambda::new(state.cur_char.map_or(V, Dot)),
                                   cont)),
        }
    }

    pub fn eval<I:io::Read, O:io::Write>(self, input: I, output: O) -> Unlambda
    {
        let mut state = UnlambdaState {
            input: input,
            output: output,
            cur_char: None,
        };
        Continuation::new().eval(self).map(|t| t.main_loop(&mut
            state)).unwrap_or_else(|u| u)
    }
}
