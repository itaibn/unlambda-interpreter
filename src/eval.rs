use std::io;
use std::ops::Deref;
use std::rc::Rc;

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
    D1Expr(Unlambda),
    D1Apply(Unlambda, Unlambda),
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
    ArgOf(Unlambda),
    AppOnExpr(Unlambda),
    AppOnVal(Unlambda),
    AppOnApply(Unlambda, Unlambda),
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

    fn eval(&self, expr: Unlambda) -> Task {
        if let Apply(ref func, ref arg) = *expr.inner {
            return Task::Eval(func.clone(),
                              self.add_part(AppOnExpr(arg.clone())));
        }
        self.throw(expr)
    }

    fn throw(&self, obj: Unlambda) -> Task {
        match self.inner {
            None => {return Task::Exit(obj);},
            Some(ref stct) => match stct.part {
                ArgOf(ref func) => Task::Apply(func.clone(), obj,
                    stct.next.clone()),
                AppOnExpr(ref arg) => if obj.is_d() {
                    stct.next.throw(Unlambda::new(D1Expr(arg.clone())))
                } else {
                    Task::Eval(arg.clone(), stct.next.add_part(ArgOf(obj)))
                },
                AppOnVal(ref arg) => if obj.is_d() {
                    // Since arg is already evaluated `d{arg} behaves in exactly
                    // the same way as arg itself. It possible to remove this
                    // branch and handle this case in `apply` by making d behave
                    // like i in an application, but I want to preserve the
                    // invariant that d is never applied on anything.
                    //
                    // This can lead to a UI problem when the interpreter also
                    // displays the result of the computation; for instance,
                    // ``ddk will confusingly output K.
                    stct.next.throw(arg.clone())
                } else {
                    Task::Apply(obj, arg.clone(), stct.next.clone())
                },
                AppOnApply(ref func, ref arg) => if obj.is_d() {
                    stct.next.throw(
                        Unlambda::new(D1Apply(func.clone(), arg.clone())))
                } else {
                    Task::Apply(func.clone(), arg.clone(),
                        stct.next.add_part(ArgOf(obj)))
                }
            }
        }
    }
}

#[derive(Clone, Debug)]
enum Task {
    Eval(Unlambda, Continuation),
    Apply(Unlambda, Unlambda, Continuation),
    Exit(Unlambda),
}

struct UnlambdaState<I:io::Read, O:io::Write> {
    input: I,
    output: O,
    cur_char: Option<char>,
    task: Task,
}

impl<I:io::Read, O:io::Write> UnlambdaState<I, O> {
    pub fn main_loop(&mut self) -> Unlambda {
        //let mut task = self;
        loop {
            /*
            match self.is_end() {
                Ok(more) => {task = more;},
                Err(result) => {return result;},
            }
            */
            //println!("{:?}", self.task);
            if let Task::Exit(ref res) = self.task {
                return res.clone();
            }
            self.single_step()
        }
    }

    fn single_step(&mut self) {
        self.task = match self.task.clone() {
            Task::Eval(expr, cont) => cont.eval(expr),
            Task::Apply(func, arg, cont) => func.apply(arg, cont, self),
            Task::Exit(_) => panic!("cannot single-step from a completed task"),
        };
    }
}

impl Unlambda {
    fn apply<I:io::Read, O:io::Write>(self, arg: Unlambda, cont: Continuation,
            state: &mut UnlambdaState<I,O>) -> Task {
        use parse;

        match *self.inner {
            Apply(_, _) => unreachable!(),
            K => cont.throw(Unlambda::new(K1(arg))),
            K1(ref c) => cont.throw(c.clone()),
            S => cont.throw(Unlambda::new(S1(arg))),
            S1(ref x) => cont.throw(Unlambda::new(S2(x.clone(), arg))),
            S2(ref x, ref y) => Task::Apply(x.clone(), arg.clone(),
                cont.add_part(AppOnApply(y.clone(), arg.clone()))),
            I => cont.throw(arg),
            V => cont.throw(Unlambda::new(V)),
            C => Task::Apply(arg, Unlambda::new(Cont(cont.clone())), cont),
            Cont(ref alt_cont) => alt_cont.clone().throw(arg),
            Dot(c) => {
                write!(state.output, "{}", c);
                cont.throw(arg)
            }
            //D => panic!("Internal error: d applied on evaluated expression."),
            // Other branches of `apply` directly return another Apply task
            // where the function is not checked to ensure that it's not `d`, it
            // is necessary to define a behavior for applying `d`. Since the
            // argument of `d` is fully evaluated `d` behaves like the identity
            // function.
            D => cont.throw(arg),
            D1Expr(ref expr) =>
                cont.add_part(ContPart::AppOnVal(arg)).eval(expr.clone()),
            D1Apply(ref func, ref x) => Task::Apply(func.clone(), x.clone(),
                                            cont.add_part(AppOnVal(arg))),
            E => Task::Exit(arg),
            At =>
                match parse::read_one_char(&mut state.input) {
                    Ok(c) => {
                        state.cur_char = Some(c);
                        Task::Apply(arg, Unlambda::new(I), cont)
                    },
                    Err(_) => {
                        state.cur_char = None;
                        Task::Apply(arg, Unlambda::new(V), cont)
                    },
            },
            Query(c) => {
                if state.cur_char == Some(c) {
                    Task::Apply(arg, Unlambda::new(I), cont)
                } else {
                    Task::Apply(arg, Unlambda::new(V), cont)
                }
            },
            Pipe => Task::Apply(arg,
                                Unlambda::new(state.cur_char.map_or(V, Dot)),
                                cont),
        }
    }

    pub fn eval<I:io::Read, O:io::Write>(self, input: I, output: O) -> Unlambda
    {
        let mut state = UnlambdaState {
            input: input,
            output: output,
            cur_char: None,
            task: Continuation::new().eval(self),
            //task: Task::Eval(self, Continuation::new()),
        };
        //Continuation::new().eval(self).map(|t| t.main_loop(&mut
        //    state)).unwrap_or_else(|u| u)
        state.main_loop()
    }
}
