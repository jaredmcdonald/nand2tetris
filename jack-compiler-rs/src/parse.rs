use std::convert::TryInto;
use tokenize::{Token, Symbol, Keyword};
use ast::*;

// get a vector of tokens from $iterator that are balanced between $open and $close, given an $initial_balance
macro_rules! balanced {
    ($iterator: expr, $initial_balance: expr, $open: pat, $close: pat) => (
        {
            let mut balance = $initial_balance;
            $iterator.by_ref().take_while(|t| {
                match t {
                    &&Token::Symbol($open) => balance += 1,
                    &&Token::Symbol($close) => balance -= 1,
                    _ => (),
                }
                balance != 0
            }).map(|t| t.clone()).collect::<Vec<_>>()
        }
    )
}

fn expect(cond: bool, msg: &str) -> Result<(), ParseError> {
    if !cond { Err(ParseError::new(msg)) } else { Ok(()) }
}

fn parse_identifier(token: &Token) -> Result<String, ParseError> {
    if let &Token::Identifier(ref id) = token {
        Ok(id.to_string())
    } else {
        Err(ParseError::new(&format!("expected identifier, found {:?}", token)))
    }
}

fn parse_class_var(tokens: &[Token]) -> Result<Var, ParseError> {
    let mut tokens_iter = tokens.iter();
    let var_type = match tokens_iter.next() {
        Some(&Token::Keyword(Keyword::Static)) => VarType::Static,
        Some(&Token::Keyword(Keyword::Field)) => VarType::Field,
        _ => return Err(ParseError::new(&format!("unexpected token in class var declaration: {:?}", tokens))),
    };

    Ok(parse_var(&tokens_iter.map(|t| t.clone()).collect::<Vec<_>>(), var_type)?)
}

fn parse_params(tokens: &[Token]) -> Result<Vec<Var>, ParseError> {
    let mut params_list = vec![];
    let mut peekable = tokens.iter().peekable();
    let err_msg = format!("malformed param list: {:?}", tokens);
    loop {
        let first = peekable.next();
        if first.is_none() {
            break;
        }
        let data_type: Type = first.unwrap().clone().try_into()?;
        let name = parse_identifier(peekable.next().ok_or(ParseError::new(&err_msg))?)?;
        params_list.push(Var { data_type, names: vec![name], var_type: VarType::Argument });
        let next = peekable.next();
        if let Some(&Token::Symbol(ref s)) = next {
            if *s == Symbol::Comma {
                expect(peekable.peek().is_some(), &err_msg)?; // trailing comma
                continue;
            }
        } else if next.is_none() {
            break;
        }
        return Err(ParseError::new(&err_msg));
    }
    Ok(params_list)
}

fn parse_var(tokens: &[Token], var_type: VarType) -> Result<Var, ParseError> {
    let err_msg = format!("malformed var declaration: {:?}", tokens);
    let mut peekable = tokens.iter().peekable();
    let data_type: Type = peekable.next().ok_or(ParseError::new(&err_msg))?.clone().try_into()?;
    let mut names = vec![];
    while let Some(name) = peekable.next() {
        names.push(parse_identifier(name)?);
        let comma_token = peekable.by_ref().next();
        if comma_token.is_none() {
            break;
        }
        expect(
            *comma_token.unwrap() == Token::Symbol(Symbol::Comma) && peekable.by_ref().peek().is_some(),
            &err_msg
        )?;
    }
    Ok(Var { names, data_type, var_type })
}

fn parse_expression(tokens: &[Token]) -> Result<Expression, ParseError> {
    Ok(Expression(parse_expression_inner(tokens)?))
}

fn parse_expression_list(tokens: &[Token]) -> Result<Vec<Expression>, ParseError> {
    if tokens.len() == 0 {
        return Ok(vec![]);
    }
    let mut tokens_iter = tokens.iter();
    let mut expression_tokens = vec![vec![]];
    while let Some(token) = tokens_iter.next() {
        if token == &Token::Symbol(Symbol::Comma) {
            expression_tokens.push(vec![])
        } else {
            let index = expression_tokens.len() - 1;
            expression_tokens[index].push(token.clone());
        }
    }
    Ok(expression_tokens.iter().map(|ts| parse_expression(ts)).collect::<Result<Vec<Expression>, ParseError>>()?)
}

fn parse_subroutine_call(first_identifier: &str, rest_tokens: &[Token]) -> Result<SubroutineCall, ParseError> {
    let mut peekable = rest_tokens.iter().peekable();
    let is_method_call = peekable.peek() == Some(&&Token::Symbol(Symbol::Period));
    let (parent_name, subroutine_name) = if is_method_call {
        (
            // in this case we're dealing with a method call, so `first_identifier` is the parent name
            Some(first_identifier.to_string()),
            // skip the period and extract the identifier, which is the method name
            parse_identifier(peekable.by_ref().skip(1).next().ok_or(
                ParseError::new("malformed subroutine call")
            )?)?
        )
    } else {
        // otherwise there is no parent and `first_identifier` is a lone function name
        (None, first_identifier.to_string())
    };

    let params_tokens = balanced!(peekable, 0, Symbol::OpenParen, Symbol::CloseParen);
    let parameters = parse_expression_list(&params_tokens[1..])?; // drop opening paren

    Ok(SubroutineCall {
        parent_name,
        subroutine_name,
        parameters,
    })
}

fn parse_expression_inner(tokens: &[Token]) -> Result<Vec<ExpressionItem>, ParseError> {
    let mut peekable = tokens.iter().peekable();
    let next = peekable.next();
    if next.is_none() {
        return Ok(vec![]);
    }
    let term = match *next.unwrap() {
        Token::IntegerConstant(ref i) => Term::IntegerConstant(*i),
        Token::StringConstant(ref s) => Term::StringConstant(s.to_string()),
        Token::Keyword(ref k) => {
            expect(
                k == &Keyword::True || k == &Keyword::False || k == &Keyword::Null || k == &Keyword::This,
                &format!("unexpected keyword `{:?}` in expression", k)
            )?;
            Term::KeywordConstant(*k)
        },
        Token::Identifier(ref id) => {
            // how to avoid this? the problem was that i need to match on the value of
            // peekable.peek and consume some more tokens within the match arms, but that's
            // borrowing as mutable twice at once per the compiler
            let mut peekable_clone = peekable.clone();
            let next = peekable_clone.peek();

            if next == Some(&&Token::Symbol(Symbol::OpenSquare)) {
                // parse index expr
                let index_expr_tokens = balanced!(peekable, 0, Symbol::OpenSquare, Symbol::CloseSquare);
                Term::IndexExpr(id.to_string(), parse_expression(&index_expr_tokens[1..])?)

            } else if next == Some(&&Token::Symbol(Symbol::OpenParen)) ||
                      next == Some(&&Token::Symbol(Symbol::Period)) {

                // grab everything before the params
                let mut call_tokens = peekable.by_ref()
                    .take_while(|t| t != &&Token::Symbol(Symbol::OpenParen))
                    .map(|t| t.clone()).collect::<Vec<Token>>();

                // grab the params
                let params_tokens = balanced!(peekable, 1, Symbol::OpenParen, Symbol::CloseParen);

                // put everything back into one list (parens included) for `parse_subroutine_call` :/
                call_tokens.push(Token::Symbol(Symbol::OpenParen));
                call_tokens.extend(params_tokens);
                call_tokens.push(Token::Symbol(Symbol::CloseParen));

                Term::SubroutineCall(parse_subroutine_call(id, &call_tokens)?)
            } else {
                // next is probably an op, or maybe this is just a lone var name
                Term::VarName(id.to_string())
            }
        },
        Token::Symbol(s) => {
            if s == Symbol::Minus || s == Symbol::Not {
                // unary op then term
                let op: UnaryOp = s.try_into()?;
                let rest = parse_expression_inner(
                    &peekable.map(|t| t.clone()).collect::<Vec<Token>>()
                )?;
                let next_term = if let ExpressionItem::Term(ref t) = rest[0] {
                    t.clone()
                } else {
                    return Err(ParseError::new("expected term after unary op"));
                };

                let mut result = vec![
                    ExpressionItem::Term(Term::Unary(op, Box::new(next_term)))
                ];

                return Ok(result);
            } else if s == Symbol::OpenParen {
                // parenthetical expression
                let parenthetical_tokens = balanced!(peekable, 1, Symbol::OpenParen, Symbol::CloseParen);
                Term::Parenthetical(parse_expression(&parenthetical_tokens)?)
            } else {
                return Err(ParseError::new(&format!("expected term, got symbol `{:?}` in expression", s)));
            }
        }
    };

    let mut expressions = vec![ExpressionItem::Term(term)];
    let op_token = peekable.next();

    if op_token.is_none() {
        return Ok(expressions); // base case
    } else {
        if let &Token::Symbol(sym) = op_token.unwrap() {
            expressions.push(ExpressionItem::Operation(sym.try_into()?));
        } else {
            return Err(ParseError::new(&format!("expected an operation, got token {:?}", op_token)));
        }
    }
    expect(peekable.peek().is_some(), "trailing operation in expression, expected a term")?;
    expressions.extend(
        parse_expression_inner(&peekable.map(|t| t.clone()).collect::<Vec<Token>>())?
    );
    Ok(expressions)
}

fn parse_let_statement(tokens: &[Token]) -> Result<LetStatement, ParseError> {
    let mut peekable = tokens.iter().peekable();
    let name = parse_identifier(peekable.next().ok_or(
        ParseError::new("missing identifier in let statement")
    )?)?;

    let index_expression = if peekable.peek() == Some(&&Token::Symbol(Symbol::OpenSquare)) {
        let index_expr_tokens = balanced!(peekable, 0, Symbol::OpenSquare, Symbol::CloseSquare);
        Some(parse_expression(&index_expr_tokens[1..])?) // omit open square bracket
    } else { None };

    // skip over the equals sign
    expect(peekable.next() == Some(&Token::Symbol(Symbol::Eq)), "missing equals sign in let statement")?;

    let expression = parse_expression(&peekable.map(|t| t.clone()).collect::<Vec<Token>>())?;
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
    let mut peekable = tokens.iter().peekable();
    let first_statement = match peekable.next() {
        Some(&Token::Keyword(Keyword::Let)) => {
            let let_tokens = peekable.by_ref()
                .take_while(|t| t != &&Token::Symbol(Symbol::Semi))
                .map(|t| t.clone()).collect::<Vec<_>>();
            Statement::Let(parse_let_statement(&let_tokens)?)
        },
        Some(&Token::Keyword(Keyword::If)) => {
            let condition_tokens = balanced!(peekable, 0, Symbol::OpenParen, Symbol::CloseParen);
            let if_body_tokens = balanced!(peekable, 0, Symbol::OpenCurly, Symbol::CloseCurly);

            let has_else_body = peekable.by_ref().peek() == Some(&&Token::Keyword(Keyword::Else));
            let else_body_tokens = if has_else_body {
                balanced!(peekable.by_ref().skip(1), 0, Symbol::OpenCurly, Symbol::CloseCurly)
            } else { vec![] };

            Statement::If(
                parse_if_statement(
                    &condition_tokens[1..],
                    &if_body_tokens[1..],
                    if has_else_body { Some(&else_body_tokens[1..]) } else { None }
                )?
            )
        },
        Some(&Token::Keyword(Keyword::While)) => {
            let condition_tokens = balanced!(peekable, 0, Symbol::OpenParen, Symbol::CloseParen);
            let body_tokens = balanced!(peekable, 0, Symbol::OpenCurly, Symbol::CloseCurly);
            Statement::While(
                parse_while_statement(&condition_tokens[1..], &body_tokens[1..])?
            )
        },
        Some(&Token::Keyword(Keyword::Do)) => {
            let first_identifier = parse_identifier(
                peekable.by_ref().next().ok_or(ParseError::new("malformed subroutine call"))?
            )?;
            let rest_tokens = peekable.by_ref()
                .take_while(|t| t != &&Token::Symbol(Symbol::Semi))
                .map(|t| t.clone()).collect::<Vec<_>>();
            Statement::Do(parse_subroutine_call(&first_identifier, &rest_tokens)?)
        },
        Some(&Token::Keyword(Keyword::Return)) => {
            let return_tokens = peekable.by_ref()
                .take_while(|t| t != &&Token::Symbol(Symbol::Semi))
                .map(|t| t.clone()).collect::<Vec<_>>();
            Statement::Return(parse_expression(&return_tokens)?)
        },
        _ => return Err(ParseError::new(&format!("unexpected keyword to begin statement: {:?}", tokens[0]))),
    };
    let mut result = vec![first_statement];
    if peekable.peek().is_some() { // more statements to be processed, recur
        result.extend(parse_statements(
            &peekable.map(|t| t.clone()).collect::<Vec<_>>()
        )?);
    }
    Ok(result)
}

fn parse_subroutine_body(tokens: &[Token]) -> Result<SubroutineBody, ParseError> {
    let mut peekable = tokens.iter().peekable();
    let mut var_declarations = vec![];
    while let Some(&&Token::Keyword(kw)) = peekable.peek() {
        if kw == Keyword::Var {
            let declaration_tokens = peekable.by_ref()
                .take_while(|t| t != &&Token::Symbol(Symbol::Semi))
                .map(|t| t.clone()).collect::<Vec<_>>();
            var_declarations.push(parse_var(&declaration_tokens[1..], VarType::Local)?);
        } else {
            break;
        }
    }

    let statements = parse_statements(&peekable.map(|t| t.clone()).collect::<Vec<_>>())?;

    Ok(SubroutineBody { var_declarations, statements })
}

fn parse_subroutine(
    subroutine_type_token: &Token,
    return_type_token: &Token,
    name_token: &Token,
    params_body: &[Token],
    subroutine_body: &[Token],
) -> Result<Subroutine, ParseError> {
    let subroutine_type: SubroutineType = if let &Token::Keyword(kw) = subroutine_type_token {
        kw.try_into()?
    } else {
        return Err(ParseError::new(&format!("expected keyword, got `{:?}`", subroutine_type_token)));
    };

    let return_type: Type = return_type_token.clone().try_into()?;
    let name = parse_identifier(name_token)?;
    let params = parse_params(params_body)?;
    let body = parse_subroutine_body(subroutine_body)?;

    Ok(Subroutine { subroutine_type, return_type, params, name, body })
}

fn parse_class_body(tokens: &[Token]) -> Result<Vec<ClassBodyItem>, ParseError> {
    let mut peekable = tokens.iter().peekable();

    if peekable.peek().is_none() {
        // base case, no tokens left
        return Ok(vec![]);
    }

    if let Some(&&Token::Keyword(ref keyword)) = peekable.peek() {
        if keyword == &Keyword::Static || keyword == &Keyword::Field {
            let class_var_tokens = peekable.by_ref()
                .take_while(|t| t != &&Token::Symbol(Symbol::Semi))
                .map(|t| t.clone()).collect::<Vec<Token>>();
            let mut result = vec![
                ClassBodyItem::ClassVar(parse_class_var(&class_var_tokens)?)
            ];
            result.extend(
                parse_class_body(&peekable.map(|t| t.clone()).collect::<Vec<Token>>())?
            );
            return Ok(result);
        } else if keyword == &Keyword::Constructor ||
                  keyword == &Keyword::Function ||
                  keyword == &Keyword::Method {

            let subroutine_type_token = peekable.next().unwrap(); // we've already peeked this token
            let return_type_token = peekable.next().ok_or(
                ParseError::new("expected return type in subroutine declaration")
            )?;
            let name_token = peekable.next().ok_or(
                ParseError::new("expected name in subroutine declaration")
            )?;

            expect(
                peekable.next() == Some(&&Token::Symbol(Symbol::OpenParen)),
                "expected `(` to open parameters list"
            )?;

            let params_tokens = peekable.by_ref()
                .take_while(|t| t != &&Token::Symbol(Symbol::CloseParen))
                .map(|t| t.clone()).collect::<Vec<Token>>();

            expect(
                peekable.next() == Some(&&Token::Symbol(Symbol::OpenCurly)),
                "expected `{` to open subroutine body"
            )?;

            let body_tokens = balanced!(peekable, 1, Symbol::OpenCurly, Symbol::CloseCurly);

            let mut result = vec![
                ClassBodyItem::Subroutine(
                    parse_subroutine(
                        &subroutine_type_token,
                        &return_type_token,
                        &name_token,
                        &params_tokens,
                        &body_tokens
                    )?
                )
            ];
            result.extend(
                parse_class_body(&peekable.map(|t| t.clone()).collect::<Vec<Token>>())?
            );
            return Ok(result);
        }
    }
    Err(ParseError::new(&format!("unexpected token in class body: {:?}", tokens[0])))
}

pub fn parse(tokens: &[Token]) -> Result<Class, ParseError> {
    let mut tokens_iter = tokens.iter();
    expect(
        tokens_iter.next() == Some(&Token::Keyword(Keyword::Class)),
        &format!("expected first token in file to be `class` keyword, got {:?}", tokens[0])
    )?;
    if let Some(&Token::Identifier(ref classname)) = tokens_iter.next() {
        let body_tokens = balanced!(tokens_iter, 0, Symbol::OpenCurly, Symbol::CloseCurly);
        Ok(Class {
            name: classname.to_string(),
            body: parse_class_body(&body_tokens[1..])?,
        })
    } else {
        Err(ParseError::new(&format!("expected an identifier after `class`, got {:?}", tokens[1])))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_parse_outer() {
        let input = [
            // class Foo {
            Token::Keyword(Keyword::Class),
            Token::Identifier("Foo".to_string()),
            Token::Symbol(Symbol::OpenCurly),
            // static int blargh, argh;
            Token::Keyword(Keyword::Static),
            Token::Keyword(Keyword::Int),
            Token::Identifier("blargh".to_string()),
            Token::Symbol(Symbol::Comma),
            Token::Identifier("argh".to_string()),
            Token::Symbol(Symbol::Semi),
            // method int blargh() {
            Token::Keyword(Keyword::Method),
            Token::Keyword(Keyword::Int),
            Token::Identifier("blargh".to_string()),
            Token::Symbol(Symbol::OpenParen),
            Token::Symbol(Symbol::CloseParen),
            Token::Symbol(Symbol::OpenCurly),
            // if (true) {
            Token::Keyword(Keyword::If),
            Token::Symbol(Symbol::OpenParen),
            Token::Keyword(Keyword::True),
            Token::Symbol(Symbol::CloseParen),
            Token::Symbol(Symbol::OpenCurly),
            // do draw();
            Token::Keyword(Keyword::Do),
            Token::Identifier("draw".to_string()),
            Token::Symbol(Symbol::OpenParen),
            Token::Symbol(Symbol::CloseParen),
            Token::Symbol(Symbol::Semi),
            // } else {
            Token::Symbol(Symbol::CloseCurly),
            Token::Keyword(Keyword::Else),
            Token::Symbol(Symbol::OpenCurly),
            // do draw2();
            Token::Keyword(Keyword::Do),
            Token::Identifier("draw2".to_string()),
            Token::Symbol(Symbol::OpenParen),
            Token::Symbol(Symbol::CloseParen),
            Token::Symbol(Symbol::Semi),
            // } // end else
            Token::Symbol(Symbol::CloseCurly),
            // } // end method
            Token::Symbol(Symbol::CloseCurly),
            // } // end class
            Token::Symbol(Symbol::CloseCurly),
        ];
        let parsed = parse(&input);
        assert!(parsed.is_ok());
    }

    #[test]
    fn test_parse_outer_errors() {
        let bad_input = [
            Token::Keyword(Keyword::Class),
            Token::Symbol(Symbol::OpenCurly),
            Token::Symbol(Symbol::CloseCurly),
        ];
        match parse(&bad_input) {
            Err(e) => assert!(e.message.starts_with("expected an identifier after `class`")),
            _ => panic!("should have been an error"),
        }

        let more_bad_input = [
            Token::Keyword(Keyword::Var),
            Token::Keyword(Keyword::Int),
            Token::Identifier("foo".to_string()),
            Token::Symbol(Symbol::Semi),
        ];
        match parse(&more_bad_input) {
            Err(e) => assert!(e.message.starts_with("expected first token in file to be `class` keyword")),
            _ => panic!("should have been an error"),
        }
    }

    #[test]
    fn test_parse_class_var() {
        let input = [
            Token::Keyword(Keyword::Static),
            Token::Keyword(Keyword::Int),
            Token::Identifier("foo".to_string()),
        ];
        assert_eq!(parse_class_var(&input).unwrap(), Var {
            var_type: VarType::Static,
            data_type: Type::Int,
            names: vec!["foo".to_string()],
        });

        let multiple_declarations = [
            Token::Keyword(Keyword::Field),
            Token::Identifier("MyCustomClass".to_string()),
            Token::Identifier("foo".to_string()),
            Token::Symbol(Symbol::Comma),
            Token::Identifier("bar".to_string()),
        ];
        assert_eq!(parse_class_var(&multiple_declarations).unwrap(), Var {
            var_type: VarType::Field,
            data_type: Type::Class("MyCustomClass".to_string()),
            names: vec!["foo".to_string(), "bar".to_string()],
        });
    }

    #[test]
    fn test_parse_let_statements() {
        let let_statement = vec![
            Token::Keyword(Keyword::Let),
            Token::Identifier("x".to_string()),
            Token::Symbol(Symbol::Eq),
            Token::IntegerConstant(2),
            Token::Symbol(Symbol::Semi),
        ];
        assert!(parse_statements(&let_statement).is_ok());

        let mut two_let_statements = vec![];
        two_let_statements.extend(let_statement.clone());
        two_let_statements.extend(let_statement.clone());
        assert!(parse_statements(&two_let_statements).is_ok());

        let complex_let = [
            Token::Keyword(Keyword::Let),
            Token::Identifier("x".to_string()),
            Token::Symbol(Symbol::OpenSquare),
            Token::IntegerConstant(2),
            Token::Symbol(Symbol::CloseSquare),
            Token::Symbol(Symbol::Eq),
            Token::IntegerConstant(2),
            Token::Symbol(Symbol::Semi),
        ];
        assert!(parse_statements(&complex_let).is_ok());
    }

    #[test]
    fn test_parse_while_statements() {
        let while_statement = [
            // while (x) {
            Token::Keyword(Keyword::While),
            Token::Symbol(Symbol::OpenParen),
            Token::Identifier("x".to_string()),
            Token::Symbol(Symbol::CloseParen),
            Token::Symbol(Symbol::OpenCurly),
            // let y = 2;
            Token::Keyword(Keyword::Let),
            Token::Identifier("y".to_string()),
            Token::Symbol(Symbol::Eq),
            Token::IntegerConstant(2),
            Token::Symbol(Symbol::Semi),
            // } // end while
            Token::Symbol(Symbol::CloseCurly),
        ];
        assert!(parse_statements(&while_statement).is_ok());
    }

    #[test]
    fn test_parse_params() {
        // empty
        assert_eq!(parse_params(&[]).unwrap(), vec![]);

        let one_param = [
            Token::Keyword(Keyword::Int),
            Token::Identifier("x".to_string()),
        ];
        assert_eq!(
            parse_params(&one_param).unwrap(),
            vec![Var { var_type: VarType::Argument, data_type: Type::Int, names: vec!["x".to_string()] }]
        );

        let three_params = [
            Token::Keyword(Keyword::Int),
            Token::Identifier("x".to_string()),
            Token::Symbol(Symbol::Comma),
            Token::Identifier("Blargh".to_string()),
            Token::Identifier("y1".to_string()),
            Token::Symbol(Symbol::Comma),
            Token::Keyword(Keyword::Char),
            Token::Identifier("y2".to_string()),
        ];
        assert_eq!(
            parse_params(&three_params).unwrap(),
            vec![
                Var { var_type: VarType::Argument, data_type: Type::Int, names: vec!["x".to_string()] },
                Var { var_type: VarType::Argument, data_type: Type::Class("Blargh".to_string()), names: vec!["y1".to_string()] },
                Var { var_type: VarType::Argument, data_type: Type::Char, names: vec!["y2".to_string()] },
            ]
        );

        let trailing_comma = &three_params[..three_params.len() - 2];
        assert!(parse_params(trailing_comma).is_err());

        let missing_identifier = &three_params[..three_params.len() - 1];
        assert!(parse_params(missing_identifier).is_err());

        let wrong_symbol = [
            Token::Keyword(Keyword::Int),
            Token::Identifier("x".to_string()),
            Token::Symbol(Symbol::Semi),
            Token::Identifier("Blargh".to_string()),
            Token::Identifier("y1".to_string()),
        ];
        assert!(parse_params(&wrong_symbol).is_err());
    }

    #[test]
    fn test_parse_subroutine_body() {
        let input = [
            // var int y, x;
            Token::Keyword(Keyword::Var),
            Token::Keyword(Keyword::Int),
            Token::Identifier("y".to_string()),
            Token::Symbol(Symbol::Comma),
            Token::Identifier("x".to_string()),
            Token::Symbol(Symbol::Semi),
            // var Blargh z;
            Token::Keyword(Keyword::Var),
            Token::Identifier("Blargh".to_string()),
            Token::Identifier("z".to_string()),
            Token::Symbol(Symbol::Semi),
            // if (x) {
            Token::Keyword(Keyword::If),
            Token::Symbol(Symbol::OpenParen),
            Token::Identifier("x".to_string()),
            Token::Symbol(Symbol::CloseParen),
            Token::Symbol(Symbol::OpenCurly),
            // return;
            Token::Keyword(Keyword::Return),
            Token::Symbol(Symbol::Semi),
            // } // end if
            Token::Symbol(Symbol::CloseCurly),
        ];
        let output = parse_subroutine_body(&input);
        println!("{:?}", output);
        assert!(output.is_ok());
    }

    #[test]
    fn test_parse_subroutine() {
        let parsed = parse_subroutine(
            &Token::Keyword(Keyword::Function),
            &Token::Keyword(Keyword::Int),
            &Token::Identifier("blargh".to_string()),
            &[
                Token::Keyword(Keyword::Int),
                Token::Identifier("x".to_string()),
            ],
            &[
                Token::Keyword(Keyword::Var),
                Token::Keyword(Keyword::Int),
                Token::Identifier("y".to_string()),
                Token::Symbol(Symbol::Semi),
                Token::Keyword(Keyword::Let),
                Token::Identifier("y".to_string()),
                Token::Symbol(Symbol::Eq),
                Token::IntegerConstant(1),
                Token::Symbol(Symbol::Semi),
                Token::Keyword(Keyword::Return),
                Token::Identifier("x".to_string()),
                Token::Symbol(Symbol::Plus),
                Token::Identifier("y".to_string()),
                Token::Symbol(Symbol::Semi),
            ]
        );
        assert!(parsed.is_ok());
    }

    #[test]
    fn test_parse_expression() {
        let parsed = parse_expression(&[
            // blargh(x[0] + 1) + 2
            Token::Identifier("blargh".to_string()),
            Token::Symbol(Symbol::OpenParen),
            Token::Identifier("x".to_string()),
            Token::Symbol(Symbol::OpenSquare),
            Token::IntegerConstant(0),
            Token::Symbol(Symbol::CloseSquare),
            Token::Symbol(Symbol::Plus),
            Token::IntegerConstant(1),
            Token::Symbol(Symbol::CloseParen),
            Token::Symbol(Symbol::Plus),
            Token::IntegerConstant(2),
        ]);
        assert!(parsed.is_ok());
    }

    #[test]
    fn test_parse_expression_list() {
        let empty = parse_expression_list(&[]);
        assert_eq!(empty.unwrap(), vec![]);

        let one_param = parse_expression_list(&[
            Token::IntegerConstant(1),
            Token::Symbol(Symbol::Plus),
            Token::IntegerConstant(2)
        ]);

        assert_eq!(one_param.unwrap(), vec![
            Expression(vec![
                ExpressionItem::Term(Term::IntegerConstant(1)),
                ExpressionItem::Operation(BinaryOp::Plus),
                ExpressionItem::Term(Term::IntegerConstant(2)),
            ])
        ]);

        let two_params = parse_expression_list(&[
            Token::IntegerConstant(1),
            Token::Symbol(Symbol::Comma),
            Token::IntegerConstant(2)
        ]);

        assert_eq!(two_params.unwrap(), vec![
            Expression(vec![
                ExpressionItem::Term(Term::IntegerConstant(1)),
            ]),
            Expression(vec![
                ExpressionItem::Term(Term::IntegerConstant(2)),
            ])
        ]);
    }
}
