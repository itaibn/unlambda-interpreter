use parse::read_token;
use std::io;

fn main() {
    let stdin = io::stdin();
    println!("{:?}", read_token(stdin));
}
