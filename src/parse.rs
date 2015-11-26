
use std::char;
use std::io;
//use std::io::ErrorKind::*;
use std::io::Read;

// This should be a standard library function. Even if it isn't it's
// better to reimplement this to use the latin1 encoding: That way an Unlambda
// program can handle arbitrary bytes in input with only 256 queries.
fn read_one_char<B: Read>(mut reader: B) -> io::Result<char> {
    let mut byte = [0];
    try!(reader.read(&mut byte));
    Ok(char::from_u32(byte[0] as u32).unwrap())
        // I think this should never happen; see test_all_bytes_valid()
//        .ok_or(
//            io::Error::new(ErrorKind::InvalidData, "Invalid char"))
}

#[cfg(test)]
#[test]
fn test_all_bytes_valid() {
    for b in 0..255 {
        assert!(char::from_u32(b).is_some());
    }
}

mod a {
    pub enum Enum{Instance}
}

fn test_enum() -> a::Enum {
    a::Enum::Instance
}

// Public for temporary test.
#[derive(Debug)]
pub enum Token {S, K, App}

// Public for temporary test.
pub fn read_token<B: Read>(mut reader: B) -> io::Result<Token> {
    let mut cur_char: char;
    loop {
        cur_char = try!(read_one_char(&mut reader));
        if cur_char.is_whitespace() {continue;}
        macro_rules! token {
            ($x: ident) => {return Ok(Token::$x)}
        }
        match cur_char {
            's' => token!(S),
            'k' => token!(K),
            '`' => token!(App),
            _ => panic!(),
        }
    }
}

type Unl = Box<UnlambdaExpr>;

enum UnlambdaExpr {
    S,
    K,
    Apply(Unl, Unl)
}
