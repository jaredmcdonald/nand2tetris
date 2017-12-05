use tokenize::Token;

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(LetStatement),
    If(IfStatement),
    While(WhileStatement),
    Do(Expression), // TODO should just be a subroutine call
    Return(Expression),
}

#[derive(Debug, PartialEq)]
pub struct Expression {
    content: Vec<Token>, // TODO
}

#[derive(Debug, PartialEq)]
pub struct WhileStatement {
    condition: Expression,
    body: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub struct IfStatement {
    condition: Expression,
    if_body: Vec<Statement>,
    else_body: Option<Vec<Statement>>,
}

#[derive(Debug, PartialEq)]
pub struct LetStatement {
    name: String,
    index_expression: Option<Expression>, // e.g. `let some_array[j + 1] = ...`
    expression: Expression,
}

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

fn find_token_index(tokens: &[Token], close: Token) -> Result<usize, ParseError> {
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

    let var = parse_var(&body[1..])?;

    Ok(ClassVar { var_type, var })
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
    let data_type = parse_type(&tokens[0])?;
    let mut names = vec![];
    for (index, token) in tokens[1..].iter().enumerate() {
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
    Ok(Var { names, data_type })
}

fn parse_expression(tokens: &[Token]) -> Result<Expression, ParseError> {
    Ok(Expression { content: tokens.to_vec() })
}

fn parse_let_statement(tokens: &[Token]) -> Result<LetStatement, ParseError> {
    let name = parse_identifier(&tokens[0])?;

    let (index_expression, eq_index) = if &tokens[1] == &Token::Symbol("[".to_string()) {
        let close_index = 1 + balance_symbol(&tokens[1..], "[", "]")?;
        (Some(parse_expression(&tokens[2..close_index])?), close_index + 1)
    } else {
        (None, 1)
    };

    if &tokens[eq_index] != &Token::Symbol("=".to_string()) {
        return Err(ParseError {
            message: format!("expected `=` in let statement, found token {:?}", tokens[eq_index])
        });
    }

    let expression = parse_expression(&tokens[eq_index + 1..])?;
    Ok(LetStatement { name, expression, index_expression })
}

fn parse_if_statement(
    condition_tokens: &[Token],
    if_body_tokens: &[Token],
    else_body_tokens: Option<&[Token]>
) -> Result<IfStatement, ParseError> {
    let condition = parse_expression(condition_tokens)?;
    let if_body = parse_statements(if_body_tokens)?;
    let else_body = if let Some(tokens) = else_body_tokens {
        Some(parse_statements(tokens)?)
    } else { None };

    Ok(IfStatement { condition, if_body, else_body })
}

// this is suspiciously similar to `parse_if_statement` 🤔
fn parse_while_statement(
    condition_tokens: &[Token],
    body_tokens: &[Token],
) -> Result<WhileStatement, ParseError> {
    Ok(WhileStatement {
        condition: parse_expression(condition_tokens)?,
        body: parse_statements(body_tokens)?
    })
}

fn parse_statements(tokens: &[Token]) -> Result<Vec<Statement>, ParseError> {
    let mut statements = vec![];
    let mut parse_index = 0;
    while parse_index < tokens.len() {
        let begin_token = &tokens[parse_index];
        if let Token::Keyword(ref kw) = tokens[parse_index] {
            match kw.as_ref() {
                "let" => {
                    let end_index = parse_index + find_token_index(&tokens[parse_index..], Token::Symbol(";".to_string()))?;
                    statements.push(
                        Statement::Let(parse_let_statement(&tokens[parse_index + 1..end_index])?)
                    );
                    parse_index = end_index + 1;
                },
                "if" => {
                    let condition_start = parse_index + 1;
                    let condition_end = condition_start + balance_symbol(&tokens[condition_start..], "(", ")")?;
                    let if_body_start = condition_end + 1;
                    let if_body_end = if_body_start + balance_symbol(&tokens[if_body_start..], "{", "}")?;
                    let (maybe_else_body, end_index) = if
                        // check to see if there are any more tokens, and if the next one is `else`
                        if_body_end + 1 < tokens.len() &&
                        &tokens[if_body_end + 1] == &Token::Keyword("else".to_string())
                    {
                        let else_body_start = if_body_end + 2; // hop over `else` token to open bracket
                        let else_body_end = else_body_start + balance_symbol(&tokens[else_body_start..], "{", "}")?;
                        (Some(&tokens[else_body_start + 1..else_body_end]), else_body_end)
                    } else {
                        (None, if_body_end)
                    };
                    statements.push(Statement::If(
                        parse_if_statement(
                            &tokens[condition_start..condition_end],
                            &tokens[if_body_start + 1..if_body_end],
                            maybe_else_body
                        )?
                    ));
                    parse_index = end_index + 1;
                },
                "while" => {
                    // TODO is there a nice way to dedupe this code from above and elsewhere?
                    let condition_start = parse_index + 1;
                    let condition_end = condition_start + balance_symbol(&tokens[condition_start..], "(", ")")?;
                    let body_start = condition_end + 1;
                    let body_end = body_start + balance_symbol(&tokens[body_start..], "{", "}")?;
                    statements.push(Statement::While(
                        parse_while_statement(
                            &tokens[condition_start..condition_end],
                            &tokens[body_start..body_end]
                        )?
                    ));
                    parse_index = body_end + 1;
                },
                "do" => {
                    let end_index = parse_index + find_token_index(&tokens[parse_index..], Token::Symbol(";".to_string()))?;
                    statements.push(Statement::Do(
                        // TODO this should be a subroutine call
                        parse_expression(&tokens[parse_index + 1..end_index])?
                    ));
                    parse_index = end_index + 1;
                },
                "return" => {
                    let end_index = parse_index + find_token_index(&tokens[parse_index..], Token::Symbol(";".to_string()))?;
                    statements.push(Statement::Return(
                        parse_expression(&tokens[parse_index + 1..end_index])?
                    ));
                    parse_index = end_index + 1;
                },
                _ => return Err(ParseError {
                    message: format!("unexpected keyword to begin statement: {:?}", begin_token)
                }),
            }
        } else {
            return Err(ParseError {
                message: format!("expected keyword to begin statement, got {:?}", begin_token)
            })
        }
    }
    Ok(statements)
}

fn parse_subroutine_body(body: &[Token]) -> Result<SubroutineBody, ParseError> {
    let mut parse_index = 0;
    let mut var_declarations = vec![];
    while parse_index < body.len() {
        let current_token = &body[parse_index];
        if current_token == &Token::Keyword("var".to_string()) {
            let declaration_start = parse_index + 1;
            let declaration_end = declaration_start +
                find_token_index(&body[declaration_start..], Token::Symbol(";".to_string()))?;
            var_declarations.push(parse_var(&body[declaration_start..declaration_end])?);
            parse_index = parse_index + declaration_end + 1;
        } else {
            break;
        }
    }

    let statements = parse_statements(&body[parse_index..])?;

    Ok(SubroutineBody { var_declarations, statements })
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
        SubroutineReturnType::Type(parse_type(&return_type_token)?)
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
    let mut class_vars = vec![];
    let mut subroutines = vec![];
    let mut parse_index = 0;
    while parse_index < body.len() {
        if let Token::Keyword(ref keyword) = body[parse_index] {
            if keyword == "static" || keyword == "field" {
                let end_index = parse_index + find_token_index(&body[parse_index..], Token::Symbol(";".to_string()))?;
                class_vars.push(parse_class_var(&body[parse_index..end_index])?);
                parse_index = end_index + 1;
            } else if keyword ==  "constructor" || keyword == "function" || keyword == "method" {
                let subroutine_type = &body[parse_index];
                let return_type = &body[parse_index + 1];
                let name = &body[parse_index + 2];
                let params_start = parse_index + 4; // + 3 is the open paren
                let params_end = params_start + find_token_index(&body[params_start..], Token::Symbol(")".to_string()))?;
                let body_start = params_end + 1;
                let body_end = body_start + balance_symbol(&body[body_start..], "{", "}")?;
                subroutines.push(
                    parse_subroutine(
                        &subroutine_type,
                        &return_type,
                        &name,
                        &body[params_start..params_end],
                        &body[body_start + 1..body_end]
                    )?
                );
                parse_index = body_end + 1;
            } else {
                return Err(ParseError {
                    message: format!("unexpected keyword in class body: {}", keyword),
                });
            }
        } else {
            return Err(ParseError {
                message: format!("unexpected token in class body: {:?}", body[parse_index]),
            });
        }
    }
    Ok(Class { name: name.to_string(), class_vars, subroutines })
}

pub fn parse(tokens: &[Token]) -> Result<Class, ParseError> {
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
    fn test_find_token_index() {
        assert_eq!(find_token_index(
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
            // class Foo {
            Token::Keyword("class".to_string()),
            Token::Identifier("Foo".to_string()),
            Token::Symbol("{".to_string()),
            // static int blargh, argh;
            Token::Keyword("static".to_string()),
            Token::Keyword("int".to_string()),
            Token::Identifier("blargh".to_string()),
            Token::Symbol(",".to_string()),
            Token::Identifier("argh".to_string()),
            Token::Symbol(";".to_string()),
            // method int blargh() {
            Token::Keyword("method".to_string()),
            Token::Keyword("int".to_string()),
            Token::Identifier("blargh".to_string()),
            Token::Symbol("(".to_string()),
            Token::Symbol(")".to_string()),
            Token::Symbol("{".to_string()),
            // if (true) {
            Token::Keyword("if".to_string()),
            Token::Symbol("(".to_string()),
            Token::Keyword("true".to_string()),
            Token::Symbol(")".to_string()),
            Token::Symbol("{".to_string()),
            // do draw();
            Token::Keyword("do".to_string()),
            Token::Identifier("draw".to_string()),
            Token::Symbol("(".to_string()),
            Token::Symbol(")".to_string()),
            Token::Symbol(";".to_string()),
            // } else {
            Token::Symbol("}".to_string()),
            Token::Keyword("else".to_string()),
            Token::Symbol("{".to_string()),
            // do draw2();
            Token::Keyword("do".to_string()),
            Token::Identifier("draw2".to_string()),
            Token::Symbol("(".to_string()),
            Token::Symbol(")".to_string()),
            Token::Symbol(";".to_string()),
            // } // end else
            Token::Symbol("}".to_string()),
            // } // end method
            Token::Symbol("}".to_string()),
            // } // end class
            Token::Symbol("}".to_string()),
        ];
        let parsed = parse(&input);
        assert!(parsed.is_ok());
    }

    #[test]
    fn test_parse_outer_errors() {
        let bad_input = vec![
            Token::Keyword("class".to_string()),
            Token::Symbol("{".to_string()),
            Token::Symbol("}".to_string()),
        ];
        match parse(&bad_input) {
            Err(e) => assert!(e.message.starts_with("expected an identifier after `class`")),
            _ => panic!("should have been an error"),
        }

        let more_bad_input = vec![
            Token::Keyword("var".to_string()),
            Token::Keyword("int".to_string()),
            Token::Identifier("foo".to_string()),
            Token::Symbol(";".to_string()),
        ];
        match parse(&more_bad_input) {
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
    fn test_parse_let_statements() {
        let let_statement = vec![
            Token::Keyword("let".to_string()),
            Token::Identifier("x".to_string()),
            Token::Symbol("=".to_string()),
            Token::IntegerConstant("2".to_string()),
            Token::Symbol(";".to_string()),
        ];
        assert!(parse_statements(&let_statement).is_ok());

        let mut two_let_statements = vec![];
        two_let_statements.extend(let_statement.clone());
        two_let_statements.extend(let_statement.clone());
        assert!(parse_statements(&two_let_statements).is_ok());

        let complex_let = vec![
            Token::Keyword("let".to_string()),
            Token::Identifier("x".to_string()),
            Token::Symbol("[".to_string()),
            Token::IntegerConstant("2".to_string()),
            Token::Symbol("]".to_string()),
            Token::Symbol("=".to_string()),
            Token::IntegerConstant("2".to_string()),
            Token::Symbol(";".to_string()),
        ];
        assert!(parse_statements(&complex_let).is_ok());
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

    #[test]
    fn test_parse_subroutine() {
        let parsed = parse_subroutine(
            &Token::Keyword("function".to_string()),
            &Token::Keyword("int".to_string()),
            &Token::Identifier("blargh".to_string()),
            &vec![
                Token::Keyword("int".to_string()),
                Token::Identifier("x".to_string()),
            ],
            &vec![
                Token::Keyword("var".to_string()),
                Token::Keyword("int".to_string()),
                Token::Identifier("y".to_string()),
                Token::Symbol(";".to_string()),
                Token::Keyword("let".to_string()),
                Token::Identifier("y".to_string()),
                Token::Symbol("=".to_string()),
                Token::IntegerConstant("1".to_string()),
                Token::Symbol(";".to_string()),
                Token::Keyword("return".to_string()),
                Token::Identifier("x".to_string()),
                Token::Symbol("+".to_string()),
                Token::Identifier("y".to_string()),
                Token::Symbol(";".to_string()),
            ]
        );
        assert!(parsed.is_ok());
    }
}
