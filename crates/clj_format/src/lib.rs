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
    fn unwrap_error<R>(input: &str, result: nom::IResult<&str, R, VerboseError<&str>>) -> R {
        match result {
            Ok(r) => r.1,
            Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
                panic!("{:?}",
                       convert_error(input, e))
            }
            _ => panic!("Unexpected result"),
        }
    }

    use nom::error::{convert_error, VerboseError};

    use crate::{edn_token, Token};

    #[test]
    fn it_works() {
        let result = 2 + 2;
        assert_eq!(result, 4);
    }

    #[test]
    fn literal_token() {
        let result = edn_token::<VerboseError<&str>>("\"hello\"");
        let result = unwrap_error("\"hello\"", result);
        assert_eq!(result, Token::String("hello".to_string()));
    }

    #[test]
    fn nested_lists() {
        let input = String::from("(\nnil\n(\ntrue\nfalse () ) )");
        let result = edn_token::<VerboseError<&str>>(&input);
        let result = unwrap_error(&input, result);
        assert_eq!(result, Token::List(vec![Token::Nil, Token::List(vec![Token::Boolean(true), Token::Boolean(false), Token::List(vec![])])]));
    }

    #[test]
    fn two_lists() {
        let input = String::from("(nil nil nil) (nil nil nil)");
        let result = crate::parse(&input);
        let result = unwrap_error(&input, result);
        assert_eq!(result, vec![Token::List(vec![Token::Nil, Token::Nil, Token::Nil]), Token::List(vec![Token::Nil, Token::Nil, Token::Nil])]);
    }

    #[test]
        fn commented_list() {
        let input = String::from(";(nil nil nil) (nil nil nil)");
        let result = edn_token::<VerboseError<&str>>(&input);
        let result = unwrap_error(&input, result);
        assert_eq!(result, Token::Comment("(nil nil nil) (nil nil nil)".to_string()));
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

fn comment<'a, E: ParseError<&'a str>>(input: &'a str) -> IResult<&'a str, &'a str, E> {
    preceded(tag(";"), take_while(|c| c != '\n'))(input)
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
            map(comment, |s| Token::Comment(String::from(s))),
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