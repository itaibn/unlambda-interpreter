extern crate unlambda_interpreter;

use unlambda_interpreter::parse::parse_expr;
use std::io;

fn main() {
    let stdin = io::stdin();
    println!("{:?}", parse_expr(stdin).unwrap().eval());
}
