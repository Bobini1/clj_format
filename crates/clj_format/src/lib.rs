use nom::{
    bytes::complete::{escaped, tag, take_while},
    character::complete::char,
    IResult,
};
use nom::branch::alt;
use nom::character::complete::{alphanumeric1, one_of};
use nom::combinator::{cut, map, value};
use nom::error::{context, ContextError, ParseError, VerboseError};
use nom::multi::{separated_list0};
use nom::sequence::{preceded, terminated};

#[cfg(test)]
mod tests {
    use nom::error::{convert_error, VerboseError};

    use crate::{edn_token, Token};

    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }

    #[test]
    fn simple_sexpr() {
        let input = String::from("(\nnil\n(\ntrue\nfalse () ) )");
        let result = match edn_token::<VerboseError<&str>>(&input) {
            Ok(r) => r,
            Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                panic!("{:?}",
                       convert_error(input.as_str(), e))
            }
            _ => panic!("Unexpected result"),
        };
        assert_eq!(result.1, Token::List(vec![Token::Nil, Token::List(vec![Token::Boolean(true), Token::Boolean(false), Token::List(vec![])])]));
    }

    #[test]
    fn two_sexpr() {
        let input = String::from("(nil nil nil) (nil nil nil)");
        let result = match crate::parse(&input) {
            Ok(r) => r,
            Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                panic!("{:?}",
                       convert_error(input.as_str(), e))
            }
            _ => panic!("Unexpected result"),
        };
        assert_eq!(result.1, vec![Token::List(vec![Token::Nil, Token::Nil, Token::Nil]), Token::List(vec![Token::Nil, Token::Nil, Token::Nil])]);
    }
}

#[derive(Debug)]
pub enum Token {
    Symbol(String),
    Vector(Vec<Token>),
    List(Vec<Token>),
    Set(Vec<Token>),
    Map(Vec<Token>),
    String(String),
    Number(String),
    Boolean(bool),
    Comment(String),
    Nil,
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Token::Symbol(a), Token::Symbol(b)) => a == b,
            (Token::Vector(a), Token::Vector(b)) => a == b,
            (Token::List(a), Token::List(b)) => a == b,
            (Token::Set(a), Token::Set(b)) => a == b,
            (Token::Map(a), Token::Map(b)) => a == b,
            (Token::String(a), Token::String(b)) => a == b,
            (Token::Number(a), Token::Number(b)) => a == b,
            (Token::Boolean(a), Token::Boolean(b)) => a == b,
            (Token::Comment(a), Token::Comment(b)) => a == b,
            (Token::Nil, Token::Nil) => true,
            _ => false,
        }
    }
}


fn sp<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    let chars = " \t\r\n:,";

    // nom combinators like `take_while` return a function. That function is the
    // parser,to which we can pass the input
    take_while(move |c| chars.contains(c))(i)
}


fn parse_str<'a, E: ParseError<&'a str>>(i: &'a str) -> IResult<&'a str, &'a str, E> {
    escaped(alphanumeric1, '\\', one_of("\"n\\"))(i)
}


fn boolean<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, bool, E> {
    // This is a parser that returns `true` if it sees the string "true", and
    // an error otherwise
    let parse_true = value(true, tag("true"));

    // This is a parser that returns `false` if it sees the string "false", and
    // an error otherwise
    let parse_false = value(false, tag("false"));

    // `alt` combines the two parsers. It returns the result of the first
    // successful parser, or an error
    alt((parse_true, parse_false))(input)
}


fn nil<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, (), E> {
    value((), tag("nil"))(input)
}


fn string<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, &'a str, E> {
    context(
        "string",
        preceded(char('\"'), cut(terminated(parse_str, char('\"')))),
    )(i)
}


fn vector<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Vec<Token>, E> {
    context(
        "vector",
        preceded(
            char('['),
            cut(terminated(
                separated_list0(sp, edn_token),
                preceded(sp, char(']')),
            )),
        ),
    )(i)
}


fn hashmap<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Vec<Token>, E> {
    context(
        "hashmap",
        preceded(
            char('{'),
            cut(terminated(
                separated_list0(sp, edn_token),
                preceded(sp, char('}')),
            )),
        ),
    )(i)
}


fn list<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Vec<Token>, E> {
    context(
        "list",
        preceded(
            char('('),
            cut(terminated(
                separated_list0(sp, edn_token),
                preceded(sp, char(')')),
            )),
        ),
    )(i)
}


fn edn_token<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Token, E> {
    preceded(
        sp,
        alt((
            map(nil, |_| Token::Nil),
            map(list, Token::List),
            map(hashmap, Token::Map),
            map(vector, Token::Vector),
            map(string, |s| Token::String(String::from(s))),
            // map(double, Token::Number),
            map(boolean, Token::Boolean),
        )),
    )(i)
}


fn root<'a, E: ParseError<&'a str> + ContextError<&'a str>>(
    i: &'a str,
) -> IResult<&'a str, Vec<Token>, E> {
    separated_list0(sp, edn_token)(i)
}

pub fn parse<'a>(s: &'a str) -> IResult<&'a str, Vec<Token>, VerboseError<&str>> {
    root::<VerboseError<&str>>(s)
}

pub fn format(s: &str) -> String {
    todo!()
}