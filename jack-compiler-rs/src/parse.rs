use tokenize::Token;

#[derive(Debug)]
pub struct Class {
    name: String,
}

#[derive(Debug)]
pub struct ParseError {
    message: String,
}

fn balance_symbol(tokens: &[Token], open: &str, close: &str) -> Result<usize, ParseError> {
    let mut balance = 1;
    if tokens[0] != Token::Symbol(open.to_string()) {
        return Err(ParseError {
            // how to maintain context so these errors are more sensible?
            message: format!("unable to balance symbols: expected {} in first position, got {:?}", open, tokens[0])
        })
    }
    for (index, token) in tokens[1..].iter().enumerate() {
        if let Token::Symbol(ref t) = *token {
            if t == open {
                balance += 1;
            } else if t == close {
                balance -= 1
            }
        }
        if balance == 0 {
            return Ok(index + 1) // loop started at index 1
        }
    }
    Err(ParseError {
        message: format!("unbalanced symbols {} and {}", open, close),
    })
}

fn parse_class(name: &str, body: &[Token]) -> Result<Class, ParseError> {
    println!("{:?}", body);
    Ok(Class { name: name.to_string() })
}

pub fn parse_outer(tokens: &[Token]) -> Result<Class, ParseError> {
    if tokens[0] == Token::Keyword("class".to_string()) {
        if let Token::Identifier(ref classname) = tokens[1] {
            let body_end = balance_symbol(&tokens[2..], "{", "}")?;
            Ok(parse_class(&classname, &tokens[3..body_end + 2])?)
        } else {
            Err(ParseError {
                message: format!("expected a classname identifier after `class`, got {:?}", tokens[1]),
            })
        }
    } else {
        Err(ParseError {
            message: format!("expected first token in file to be `class` keyword, got {:?}", tokens[0]),
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_balance_symbol_simple() {
        assert_eq!(balance_symbol(
            vec![
                Token::Symbol("{".to_string()),
                Token::Keyword("do".to_string()),
                Token::Identifier("blargh".to_string()),
                Token::Symbol("(".to_string()),
                Token::Symbol(")".to_string()),
                Token::Symbol(";".to_string()),
                Token::Symbol("}".to_string()),
            ].as_slice(),
            "{",
            "}"
        ).unwrap(), 6);
    }

    #[test]
    fn test_balance_symbol_complex() {
        assert_eq!(balance_symbol(
            vec![
                Token::Symbol("{".to_string()),
                Token::Symbol("{".to_string()),
                Token::Keyword("return".to_string()),
                Token::IntegerConstant("1".to_string()),
                Token::Symbol(";".to_string()),
                Token::Symbol("}".to_string()),
                Token::Symbol("}".to_string()),
            ].as_slice(),
            "{",
            "}"
        ).unwrap(), 6);
    }

    #[test]
    fn test_parse_outer() {
        let input = vec![
            Token::Keyword("class".to_string()),
            Token::Identifier("Foo".to_string()),
            Token::Symbol("{".to_string()),
            Token::Keyword("static".to_string()),
            Token::Keyword("int".to_string()),
            Token::Identifier("blargh".to_string()),
            Token::Symbol(";".to_string()),
            Token::Symbol("}".to_string()),
        ];
        assert!(parse_outer(&input).is_ok());
    }

    #[test]
    fn test_parse_outer_errors() {
        let bad_input = vec![
            Token::Keyword("class".to_string()),
            Token::Symbol("{".to_string()),
            Token::Symbol("}".to_string()),
        ];
        match parse_outer(&bad_input) {
            Err(e) => assert!(e.message.starts_with("expected a classname identifier after `class`")),
            _ => panic!("should have been an error"),
        }

        let more_bad_input = vec![
            Token::Keyword("var".to_string()),
            Token::Keyword("int".to_string()),
            Token::Identifier("foo".to_string()),
            Token::Symbol(";".to_string()),
        ];
        match parse_outer(&more_bad_input) {
            Err(e) => assert!(e.message.starts_with("expected first token in file to be `class` keyword")),
            _ => panic!("should have been an error"),
        }
    }
}
