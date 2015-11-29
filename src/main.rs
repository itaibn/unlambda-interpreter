extern crate unlambda_interpreter;

use unlambda_interpreter::parse::parse_expr;
use std::io;

fn main() {
    let mut input = io::stdin();
    let output = io::stdout();
    let expr = parse_expr(&mut input).expect("Parsing error");
    expr.eval(input, output);
    //println!("{:?}", parse_expr(stdin).unwrap().eval());
}
