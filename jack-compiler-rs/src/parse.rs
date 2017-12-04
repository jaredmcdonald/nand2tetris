use tokenize::Token;

#[derive(Debug, PartialEq)]
pub struct Subroutine {}

#[derive(Debug, PartialEq)]
pub enum ClassVarType {
    Field,
    Static,
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Int,
    Char,
    Boolean,
    Class(String),
}

#[derive(Debug, PartialEq)]
pub struct ClassVar {
    var_type: ClassVarType,
    data_type: Type,
    names: Vec<String>, // can declare more than one at once, e.g. `static int x, y;`
}

#[derive(Debug, PartialEq)]
pub struct Class {
    name: String,
    class_vars: Vec<ClassVar>,
    subroutines: Vec<Subroutine>,
}

#[derive(Debug, PartialEq)]
pub struct ParseError {
    message: String,
}

fn tokens_until(tokens: &[Token], close: Token) -> Result<usize, ParseError> {
    for (index, token) in tokens.iter().enumerate() {
        if *token == close {
            return Ok(index)
        }
    }
    Err(ParseError {
        message: format!("could not find closing symbol {:?}", close),
    })
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

fn parse_class_var(body: &[Token]) -> Result<ClassVar, ParseError> {
    let var_type = match body[0] {
        Token::Keyword(ref kw) => match kw.as_ref() {
            "static" => ClassVarType::Static,
            "field" => ClassVarType::Field,
            _ => return Err(ParseError { message: format!("unexpected keyword in class var declaration: {}", kw) }),
        },
        _ => return Err(ParseError { message: format!("unexpected token in class var declaration: {:?}", body[0]) }),
    };
    let data_type = match body[1] {
        Token::Keyword(ref kw) => match kw.as_ref() {
            "int" => Type::Int,
            "char" => Type::Char,
            "boolean" => Type::Boolean,
            _ => return Err(ParseError { message: format!("expected a type for class var, got keyword {:?}", kw) }),
        },
        Token::Identifier(ref id) => Type::Class(id.to_string()),
        _ => return Err(ParseError { message: format!("expected a type for class var, got {:?}", body[1]) }),
    };

    let mut names = vec![];
    for (index, token) in body[2..].iter().enumerate() {
        if index % 2 != 0 {
            if token == &Token::Symbol(",".to_string()) {
                continue;
            } else {
                return Err(ParseError { message: format!("expected `,`, found {:?}", token) });
            }
        } else {
            if let &Token::Identifier(ref id) = token {
                names.push(id.to_string());
            } else {
                return Err(ParseError { message: format!("expected identifier, found {:?}", token) });
            }
        }
    }

    Ok(ClassVar {
        var_type,
        data_type,
        names,
    })
}

fn parse_class(name: &str, body: &[Token]) -> Result<Class, ParseError> {
    let mut current_token_index = 0;
    let mut class_vars = vec![];
    let mut subroutines = vec![];
    while current_token_index < body.len() {
        if let Token::Keyword(ref keyword) = body[current_token_index] {
            if keyword == "static" || keyword == "field" {
                let end_index = tokens_until(&body[current_token_index..], Token::Symbol(";".to_string()))?;
                class_vars.push(parse_class_var(&body[current_token_index..current_token_index + end_index])?);
                current_token_index = end_index + 1;
            } else {
                return Err(ParseError {
                    message: format!("unexpected keyword in class body: {}", keyword),
                });
            }
        } else {
            return Err(ParseError {
                message: format!("unexpected token in class body: {:?}", body[current_token_index]),
            });
        }
    }
    Ok(Class { name: name.to_string(), class_vars, subroutines })
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
    fn test_tokens_until() {
        assert_eq!(tokens_until(
            vec![
                Token::Keyword("static".to_string()),
                Token::Keyword("int".to_string()),
                Token::Identifier("a".to_string()),
                Token::Symbol(",".to_string()),
                Token::Symbol("b".to_string()),
                Token::Symbol(";".to_string()),
                Token::Symbol("constructor".to_string()),
            ].as_slice(),
            Token::Symbol(";".to_string()),
        ).unwrap(), 5);
    }

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

    #[test]
    fn test_parse_class_var() {
        let input = vec![
            Token::Keyword("static".to_string()),
            Token::Keyword("int".to_string()),
            Token::Identifier("foo".to_string()),
        ];
        assert_eq!(parse_class_var(&input).unwrap(), ClassVar {
            var_type: ClassVarType::Static,
            data_type: Type::Int,
            names: vec!["foo".to_string()],
        });

        let multiple_declarations = vec![
            Token::Keyword("field".to_string()),
            Token::Identifier("MyCustomClass".to_string()),
            Token::Identifier("foo".to_string()),
            Token::Symbol(",".to_string()),
            Token::Identifier("bar".to_string()),
        ];
        assert_eq!(parse_class_var(&multiple_declarations).unwrap(), ClassVar {
            var_type: ClassVarType::Field,
            data_type: Type::Class("MyCustomClass".to_string()),
            names: vec!["foo".to_string(), "bar".to_string()],
        });
    }
}
