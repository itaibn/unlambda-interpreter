
use std::char;
use std::io::{self, BufRead, BufReader, Read};

use eval::{Unlambda, UnlambdaEnum};

#[derive(Debug)]
pub enum ParseError {
    IOError(io::Error),
    InvalidToken(char),
}

impl From<io::Error> for ParseError {
    fn from(err: io::Error) -> ParseError {
        ParseError::IOError(err)
    }
}

// This should be a standard library function. Even if it isn't it's
// better to reimplement this to use the latin1 encoding: That way an Unlambda
// program can handle arbitrary bytes in input with only 256 queries.
fn read_one_char<B: Read>(mut reader: B) -> io::Result<char> {
    let mut byte = [0];
    //try!(reader.read(&mut byte).map_err(|err| IOError(err)));
    try!(reader.read(&mut byte));
    char::from_u32(byte[0] as u32)
        // I think this should never happen; see test_all_bytes_valid()
        .ok_or(
            io::Error::new(io::ErrorKind::InvalidData, "Invalid char"))
}

#[cfg(test)]
#[test]
fn test_all_bytes_valid() {
    for b in 0..255 {
        assert!(char::from_u32(b).is_some());
    }
}

// Public for temporary test.
#[derive(Debug)]
pub enum Token {App, K, S, I, V, C, Dot(char), E, At, Query(char), Pipe}

// Public for temporary test.
pub fn read_token<B: Read>(mut reader: B) -> Result<Token, ParseError> {
    use self::Token::*;

    let mut cur_char: char;
    loop {
        cur_char = try!(read_one_char(&mut reader));
        if cur_char.is_whitespace() {continue;}
        if cur_char == '#' {
            let mut buf_reader = BufReader::new(reader);
            let mut comment = String::new(); // To be dropped later
            try!(buf_reader.read_line(&mut comment));
            reader = buf_reader.into_inner();
            continue;
        }
        break;
    }

    match cur_char {
        '`' => Ok(App),
        'k' => Ok(K),
        's' => Ok(S),
        'i' => Ok(I),
        'v' => Ok(V),
        'c' => Ok(C),
        'e' => Ok(E),
        '@' => Ok(At),
        '|' => Ok(Pipe),
        '.' => {
            cur_char = try!(read_one_char(&mut reader));
            Ok(Dot(cur_char))
        },
        '?' => {
            cur_char = try!(read_one_char(&mut reader));
            Ok(Query(cur_char))
        },
        _ => Err(ParseError::InvalidToken(cur_char)),
    }
}

enum TokenAsExpr {
    App,
    Expr(UnlambdaEnum)
}

macro_rules! mk_token_to_expr {
    {
    simple {$($s:ident)*}
    char_arg {$($c:ident)*}
    }
    => {
    fn token_to_expr(token: Token) -> TokenAsExpr {
        match token {
            Token::App => TokenAsExpr::App,
            $(
            Token::$s => TokenAsExpr::Expr(UnlambdaEnum::$s),
            )*
            $(
            Token::$c(ch) => TokenAsExpr::Expr(UnlambdaEnum::$c(ch)),
            )*
        }
    }
}}

mk_token_to_expr! {
    simple {K S I V C E At Pipe}
    char_arg {Dot Query}
}

pub fn parse_expr<B:Read>(mut reader: B) -> Result<Unlambda, ParseError> {
    enum ParseCtx {AppFn, AppArg(Unlambda)}

    let mut parse_stack: Vec<ParseCtx> = Vec::new();

    loop {
        let token = try!(read_token(&mut reader));
        match token_to_expr(token) {
            TokenAsExpr::App => {parse_stack.push(ParseCtx::AppFn);},
            TokenAsExpr::Expr(x) => {
                let mut arg = Unlambda::new(x);
                loop {
                    match parse_stack.pop() {
                        None => {return Ok(arg);},
                        Some(ParseCtx::AppFn) => {
                            parse_stack.push(ParseCtx::AppArg(arg));
                            break;
                        }
                        Some(ParseCtx::AppArg(func)) => {
                            arg = Unlambda::new(UnlambdaEnum::Apply(func, arg));
                        }
                    }
                }
            }
        }
    }
}
