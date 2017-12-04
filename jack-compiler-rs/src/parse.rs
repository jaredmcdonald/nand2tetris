use tokenize::Token;

#[derive(Debug, PartialEq)]
pub enum SubroutineType {
    Constructor,
    Function,
    Method,
}

#[derive(Debug, PartialEq)]
pub enum SubroutineReturnType {
    Void,
    Type(Type),
}

#[derive(Debug, PartialEq)]
pub struct Param {
    param_type: Type,
    name: String,
}

#[derive(Debug, PartialEq)]
pub struct Statement {}

#[derive(Debug, PartialEq)]
pub struct SubroutineBody {
    var_declarations: Vec<Var>,
    statements: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub struct Subroutine {
    subroutine_type: SubroutineType,
    return_type: SubroutineReturnType,
    params: Vec<Param>,
    name: String,
    body: SubroutineBody,
}

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
    var: Var,
}

#[derive(Debug, PartialEq)]
pub struct Var {
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

fn parse_type(token: &Token) -> Result<Type, ParseError> {
    match token {
        &Token::Keyword(ref kw) => match kw.as_ref() {
            "int" => Ok(Type::Int),
            "char" => Ok(Type::Char),
            "boolean" => Ok(Type::Boolean),
            _ => Err(ParseError { message: format!("expected a type, got keyword {:?}", kw) }),
        },
        &Token::Identifier(ref id) => Ok(Type::Class(id.to_string())),
        _ => Err(ParseError { message: format!("expected a type, got token {:?}", token) }),
    }
}

fn parse_identifier(token: &Token) -> Result<String, ParseError> {
    if let &Token::Identifier(ref id) = token {
        Ok(id.to_string())
    } else {
        Err(ParseError { message: format!("expected identifier, found {:?}", token) })
    }
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

    let data_type = parse_type(&body[1])?;

    let mut names = vec![];
    for (index, token) in body[2..].iter().enumerate() {
        if index % 2 != 0 {
            if token == &Token::Symbol(",".to_string()) {
                continue;
            } else {
                return Err(ParseError { message: format!("expected `,`, found {:?}", token) });
            }
        } else {
            names.push(parse_identifier(&token)?);
        }
    }

    Ok(ClassVar { var_type, var: Var { data_type, names }})
}

fn parse_params(tokens: &[Token]) -> Result<Vec<Param>, ParseError> {
    let mut i = 0;
    let mut params_list = vec![];
    let len = tokens.len();
    let err = ParseError { message: format!("malformed param list: {:?}", tokens) };
    while i < len { // this is just terrible
        if len - i < 2 {
            return Err(err); // too few tokens
        }

        let param_type = parse_type(&tokens[i])?;
        let name = parse_identifier(&tokens[i + 1])?;
        params_list.push(Param { param_type, name });

        if len - i == 2 {
            break; // done!
        } else if tokens[i + 2] == Token::Symbol(",".to_string()) {
            if len > i + 3 {
                i += 3; // advance the loop
            } else {
                return Err(err);
            }
        } else {
            return Err(err);
        }
    }
    Ok(params_list)
}

fn parse_var(tokens: &[Token]) -> Result<Var, ParseError> {
    Err(ParseError { message: "argh".to_string() })
}

fn parse_subroutine_body(body: &[Token]) -> Result<SubroutineBody, ParseError> {
    let mut parse_index = 0;
    let mut var_declarations = vec![];
    while parse_index < body.len() {
        if body[parse_index] == Token::Keyword("var".to_string()) {
            let declaration_end = tokens_until(&body[parse_index..], Token::Symbol(";".to_string()))?;
            var_declarations.push(parse_var(&body[parse_index..parse_index + declaration_end]));
            parse_index = parse_index + declaration_end + 1;
        }
    }
    Err(ParseError {message: "unimplemented!".to_string()})
}

fn parse_subroutine(
    subroutine_type_token: &Token,
    return_type_token: &Token,
    name_token: &Token,
    params_body: &[Token],
    subroutine_body: &[Token],
) -> Result<Subroutine, ParseError> {
    let subroutine_type_err = ParseError {
        message: format!("expected `constructor`, `function` or `method`, got {:?}", subroutine_type_token),
    };
    let subroutine_type = if let &Token::Keyword(ref kw) = subroutine_type_token {
        match kw.as_ref() {
            "function" => SubroutineType::Function,
            "constructor" => SubroutineType::Constructor,
            "method" => SubroutineType::Method,
            _ => return Err(subroutine_type_err),
        }
    } else {
        return Err(subroutine_type_err);
    };

    let return_type = if return_type_token == &Token::Keyword("void".to_string()) {
        SubroutineReturnType::Void
    } else {
        SubroutineReturnType::Type(parse_type(&subroutine_type_token)?)
    };

    let name = parse_identifier(name_token)?;
    let params = parse_params(params_body)?;
    let body = parse_subroutine_body(subroutine_body)?;

    Ok(Subroutine {
        subroutine_type,
        return_type,
        params,
        name,
        body,
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
            } else if keyword ==  "constructor" || keyword == "function" || keyword == "method" {
                let subroutine_type = &body[current_token_index + 1];
                let return_type = &body[current_token_index + 2];
                let name = &body[current_token_index + 3];
                let params_start = current_token_index + 4;
                let params_end = tokens_until(&body[params_start..], Token::Symbol(")".to_string()))?;
                let body_start = params_end + 1;
                let body_end = balance_symbol(&body[body_start..], "{", "}")?;
                subroutines.push(
                    parse_subroutine(
                        &subroutine_type,
                        &return_type,
                        &name,
                        &body[params_start + 1..params_end],
                        &body[body_start + 1..body_end]
                    )?
                )
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
                message: format!("expected an identifier after `class`, got {:?}", tokens[1]),
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
            Err(e) => assert!(e.message.starts_with("expected an identifier after `class`")),
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
            var: Var {
                data_type: Type::Int,
                names: vec!["foo".to_string()],
            },
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
            var: Var {
                data_type: Type::Class("MyCustomClass".to_string()),
                names: vec!["foo".to_string(), "bar".to_string()],
            },
        });
    }

    #[test]
    fn test_parse_params() {
        // empty
        assert_eq!(parse_params(&vec![]).unwrap(), vec![]);

        let one_param = vec![
            Token::Keyword("int".to_string()),
            Token::Identifier("x".to_string()),
        ];
        assert_eq!(
            parse_params(&one_param).unwrap(),
            vec![Param { param_type: Type::Int, name: "x".to_string() }]
        );

        let three_params = vec![
            Token::Keyword("int".to_string()),
            Token::Identifier("x".to_string()),
            Token::Symbol(",".to_string()),
            Token::Identifier("Blargh".to_string()),
            Token::Identifier("y1".to_string()),
            Token::Symbol(",".to_string()),
            Token::Keyword("char".to_string()),
            Token::Identifier("y2".to_string()),
        ];
        assert_eq!(
            parse_params(&three_params).unwrap(),
            vec![
                Param { param_type: Type::Int, name: "x".to_string() },
                Param { param_type: Type::Class("Blargh".to_string()), name: "y1".to_string() },
                Param { param_type: Type::Char, name: "y2".to_string() },
            ]
        );

        let trailing_comma = &three_params[..three_params.len() - 2];
        assert!(parse_params(trailing_comma).is_err());

        let missing_identifier = &three_params[..three_params.len() - 1];
        assert!(parse_params(missing_identifier).is_err());

        let wrong_symbol = vec![
            Token::Keyword("int".to_string()),
            Token::Identifier("x".to_string()),
            Token::Symbol(";".to_string()),
            Token::Identifier("Blargh".to_string()),
            Token::Identifier("y1".to_string()),
        ];
        assert!(parse_params(&wrong_symbol).is_err());
    }
}
