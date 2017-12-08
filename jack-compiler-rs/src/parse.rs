use std::fmt;
use std::convert::TryInto;
use tokenize::{Token, Symbol, Keyword};

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(LetStatement),
    If(IfStatement),
    While(WhileStatement),
    Do(SubroutineCall), // TODO should just be a subroutine call
    Return(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Statement::Let(ref s) => write!(f, "<letStatement>\n{}\n</letStatement>", s),
            Statement::If(ref s) => write!(f, "<ifStatement>\n{}\n</ifStatement>", s),
            Statement::While(ref s) => write!(f, "<whileStatement>\n{}\n</whileStatement>", s),
            Statement::Do(ref s) => {
                write!(f,
                    "<doStatement>
                        <keyword>do</keyword>
                        {}<symbol>;</symbol>
                    </doStatement>",
                    s
                )
            },
            Statement::Return(ref s) => {
                let maybe_expr = if s.0.len() > 0 { format!("\n{}", s) } else { "".to_string() };
                write!(f,
                    "<returnStatement>
                        <keyword>return</keyword>{}
                        <symbol>;</symbol>
                    </returnStatement>",
                    maybe_expr
                )
            },
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            &UnaryOp::Neg => Token::Symbol(Symbol::Minus),
            &UnaryOp::Not => Token::Symbol(Symbol::Not),
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOp {
    Plus,
    Minus,
    Mult,
    Div,
    And,
    Or,
    Lt,
    Gt,
    Eq,
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            &BinaryOp::Plus => Token::Symbol(Symbol::Plus),
            &BinaryOp::Minus => Token::Symbol(Symbol::Minus),
            &BinaryOp::Mult => Token::Symbol(Symbol::Mult),
            &BinaryOp::Div => Token::Symbol(Symbol::Div),
            &BinaryOp::And => Token::Symbol(Symbol::Amp),
            &BinaryOp::Or => Token::Symbol(Symbol::Pipe),
            &BinaryOp::Lt => Token::Symbol(Symbol::Lt),
            &BinaryOp::Gt => Token::Symbol(Symbol::Gt),
            &BinaryOp::Eq => Token::Symbol(Symbol::Eq),
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct SubroutineCall {
    parent_name: Option<String>,
    subroutine_name: String,
    parameters: Vec<Expression>,
}

impl fmt::Display for SubroutineCall {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let parent_prefix = if let Some(ref p) = self.parent_name {
            format!("<identifier>{}</identifier>\n<symbol>.</symbol>\n", p)
        } else { "".to_string() };
        let param_list = self.parameters.iter()
            .map(|expr| format!("{}\n", expr))
            .collect::<Vec<String>>().join("<symbol>,</symbol>\n");
        write!(f, "{}<identifier>{}</identifier>
        <symbol>(</symbol>
        <expressionList>
        {}</expressionList>
        <symbol>)</symbol>
        ", parent_prefix, self.subroutine_name, param_list)
    }
}

impl TryInto<BinaryOp> for Symbol {
    type Error = ParseError;

    fn try_into(self) -> Result<BinaryOp, Self::Error> {
        match self {
            Symbol::Plus => Ok(BinaryOp::Plus),
            Symbol::Minus => Ok(BinaryOp::Minus),
            Symbol::Mult => Ok(BinaryOp::Mult),
            Symbol::Div => Ok(BinaryOp::Div),
            Symbol::Amp => Ok(BinaryOp::And),
            Symbol::Pipe => Ok(BinaryOp::Or),
            Symbol::Lt => Ok(BinaryOp::Lt),
            Symbol::Gt => Ok(BinaryOp::Gt),
            Symbol::Eq => Ok(BinaryOp::Eq),
            _ => Err(Self::Error { message: format!("unrecognized operation `{}`", self) }),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExpressionItem {
    Term(Term),
    Operation(BinaryOp),
}

impl fmt::Display for ExpressionItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ExpressionItem::Term(ref t) => write!(f, "{}", t),
            &ExpressionItem::Operation(ref o) => write!(f, "{}", o),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Term {
    IntegerConstant(u16),
    StringConstant(String),
    KeywordConstant(Keyword),
    VarName(String),
    Unary(UnaryOp, Box<Term>), // can i do this?
    Parenthetical(Expression),
    IndexExpr(String, Expression),
    SubroutineCall(SubroutineCall),
}

impl fmt::Display for Term {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<term>\n{}</term>", match self {
            &Term::IntegerConstant(ref i) => format!("<integerConstant>{}</integerConstant>\n", i),
            &Term::StringConstant(ref s) => format!("<stringConstant>{}</stringConstant>\n", s),
            &Term::KeywordConstant(ref k) => format!("<keyword>{}</keyword>\n", k),
            &Term::VarName(ref v) => format!("<identifier>{}</identifier>\n", v),
            &Term::Unary(ref op, ref term) => format!("{}\n{}\n", op, term),
            &Term::Parenthetical(ref e) => format!("<symbol>(</symbol>\n{}\n<symbol>)</symbol>\n", e),
            &Term::IndexExpr(ref var_name, ref expr) => format!(
                    "<identifier>{}</identifier>
                    <symbol>[</symbol>
                    {}
                    <symbol>]</symbol>\n",
                var_name, expr),
            &Term::SubroutineCall(ref s) => format!("{}", s),
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Expression(Vec<ExpressionItem>);

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let content = self.0.iter().map(|t| format!("{}\n", t)).collect::<String>();
        write!(f, "<expression>\n{}</expression>", content)
    }
}

#[derive(Debug, PartialEq)]
pub struct WhileStatement {
    condition: Expression,
    body: Vec<Statement>,
}

impl fmt::Display for WhileStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let body = self.body.iter().map(|t| format!("{}", t)).collect::<Vec<_>>().join("\n");
        write!(f,
            "<keyword>while</keyword>
            <symbol>(</symbol>
            {}
            <symbol>)</symbol>
            <symbol>{{</symbol>
            <statements>
                {}
            </statements>
            <symbol>}}</symbol>",
            self.condition,
            body
        )
    }
}

#[derive(Debug, PartialEq)]
pub struct IfStatement {
    condition: Expression,
    if_body: Vec<Statement>,
    else_body: Option<Vec<Statement>>,
}

impl fmt::Display for IfStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let if_body = self.if_body.iter().map(|t| format!("{}", t)).collect::<Vec<_>>().join("\n");
        let maybe_else = if let Some(ref else_body) = self.else_body {
            format!(
                "\n<keyword>else</keyword>
                <symbol>{{</symbol>
                <statements>
                    {}
                </statements>
                <symbol>}}</symbol>",
                else_body.iter().map(|t| format!("{}", t)).collect::<Vec<_>>().join("\n")
            )
        } else { "".to_string() };
        write!(f,
            "<keyword>if</keyword>
            <symbol>(</symbol>
            {}
            <symbol>)</symbol>
            <symbol>{{</symbol>
            <statements>
                {}
            </statements>
            <symbol>}}</symbol>{}",
            self.condition,
            if_body,
            maybe_else
        )
    }
}

#[derive(Debug, PartialEq)]
pub struct LetStatement {
    name: String,
    index_expression: Option<Expression>, // e.g. `let some_array[j + 1] = ...`
    expression: Expression,
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let maybe_index_expression = if let Some(ref index_expr) = self.index_expression {
            format!(
                "\n<symbol>[</symbol>
                    {}
                 <symbol>]</symbol>", index_expr)
        } else { "".to_string() };
        write!(f, "<keyword>let</keyword>
            <identifier>{}</identifier>{}
            <symbol>=</symbol>
            {}
            <symbol>;</symbol>",
            self.name,
            maybe_index_expression,
            self.expression
        )
    }
}

#[derive(Debug, PartialEq)]
pub enum SubroutineType {
    Constructor,
    Function,
    Method,
}

impl fmt::Display for SubroutineType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<keyword>{}</keyword>", match self {
            &SubroutineType::Constructor => "constructor",
            &SubroutineType::Function => "function",
            &SubroutineType::Method => "method",
        })
    }
}

#[derive(Debug, PartialEq)]
pub enum SubroutineReturnType {
    Void,
    Type(Type),
}

impl fmt::Display for SubroutineReturnType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &SubroutineReturnType::Void => write!(f, "<keyword>void</keyword>"),
            &SubroutineReturnType::Type(ref t) => write!(f, "{}", t),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Param {
    param_type: Type,
    name: String,
}

impl fmt::Display for Param {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}\n<identifier>{}</identifier>", self.param_type, self.name)
    }
}

#[derive(Debug, PartialEq)]
pub struct SubroutineBody {
    var_declarations: Vec<Var>,
    statements: Vec<Statement>,
}

impl fmt::Display for SubroutineBody {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let var_declarations = self.var_declarations.iter().map(|v| {
            format!(
                "\n<varDec>
                    <keyword>var</keyword>
                    {}
                    <symbol>;</symbol>
                </varDec>",
                v
            )
        }).collect::<String>();
        let statements = self.statements.iter().map(|s| format!("{}", s)).collect::<Vec<_>>().join("\n");
        write!(f,
            "<subroutineBody>
                <symbol>{{</symbol>{}
                    <statements>
                    {}
                    </statements>
                <symbol>}}</symbol>
            </subroutineBody>",
            var_declarations,
            statements
        )
    }
}

#[derive(Debug, PartialEq)]
pub struct Subroutine {
    subroutine_type: SubroutineType,
    return_type: SubroutineReturnType,
    params: Vec<Param>,
    name: String,
    body: SubroutineBody,
}

impl fmt::Display for Subroutine {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let param_list_body = self.params.iter().map(|p| format!("{}\n", p))
            .collect::<Vec<String>>()
            .join("<symbol>,</symbol>\n");

        write!(f,
            "<subroutineDec>
                {}
                {}
                <identifier>{}</identifier>
                <symbol>(</symbol>
                    <parameterList>
                    {}</parameterList>
                <symbol>)</symbol>
                {}
            </subroutineDec>",
            self.subroutine_type,
            self.return_type,
            self.name,
            param_list_body,
            self.body
        )
    }
}

#[derive(Debug, PartialEq)]
pub enum ClassVarType {
    Field,
    Static,
}

impl fmt::Display for ClassVarType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<keyword>{}</keyword>", match self {
            &ClassVarType::Field => "field",
            &ClassVarType::Static => "static",
        })
    }
}

#[derive(Debug, PartialEq)]
pub enum ClassBodyItem {
    ClassVar(ClassVar),
    Subroutine(Subroutine),
}

impl fmt::Display for ClassBodyItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &ClassBodyItem::ClassVar(ref cv) => write!(f, "{}", cv),
            &ClassBodyItem::Subroutine(ref sr) => write!(f, "{}", sr),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Int,
    Char,
    Boolean,
    Class(String),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Type::Int => write!(f, "<keyword>int</keyword>"),
            &Type::Char => write!(f, "<keyword>char</keyword>"),
            &Type::Boolean => write!(f, "<keyword>boolean</keyword>"),
            &Type::Class(ref s) => write!(f, "<identifier>{}</identifier>", s),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ClassVar {
    var_type: ClassVarType,
    var: Var,
}

impl fmt::Display for ClassVar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f,
            "<classVarDec>
                {}
                {}
                <symbol>;</symbol>
            </classVarDec>",
            self.var_type,
            self.var
        )
    }
}

#[derive(Debug, PartialEq)]
pub struct Var {
    data_type: Type,
    names: Vec<String>, // can declare more than one at once, e.g. `static int x, y;`
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let names = self.names.iter().map(|n| format!("<identifier>{}</identifier>", n))
            .collect::<Vec<String>>()
            .join("\n<symbol>,</symbol>\n");
        write!(f, "{}\n{}", self.data_type, names)
    }
}

#[derive(Debug, PartialEq)]
pub struct Class {
    name: String,
    body: Vec<ClassBodyItem>,
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let body = self.body.iter().map(|item| format!("{}", item))
            .collect::<Vec<String>>()
            .join("\n");
        write!(f,
            "<class>
                <keyword>class</keyword>
                <identifier>{}</identifier>
                <symbol>{{</symbol>
                    {}
                <symbol>}}</symbol>
            </class>",
            self.name,
            body
        )
    }
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

fn balance_symbol(tokens: &[Token], open: Symbol, close: Symbol) -> Result<usize, ParseError> {
    let mut balance = 1;
    if tokens[0] != Token::Symbol(open) {
        return Err(ParseError {
            // how to maintain context so these errors are more sensible?
            message: format!("unable to balance symbols: expected {} in first position, got {:?}", open, tokens[0])
        })
    }
    for (index, token) in tokens[1..].iter().enumerate() {
        if let Token::Symbol(ref t) = *token {
            if t == &open {
                balance += 1;
            } else if t == &close {
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
        &Token::Keyword(Keyword::Int) => Ok(Type::Int),
        &Token::Keyword(Keyword::Char) => Ok(Type::Char),
        &Token::Keyword(Keyword::Boolean) => Ok(Type::Boolean),
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
        Token::Keyword(Keyword::Static) => ClassVarType::Static,
        Token::Keyword(Keyword::Field) => ClassVarType::Field,
        _ => return Err(ParseError { message: format!("unexpected token in class var declaration: {:?}", body[0]) }),
    };

    let var = parse_var(&body[1..])?;

    Ok(ClassVar { var_type, var })
}

fn parse_params(tokens: &[Token]) -> Result<Vec<Param>, ParseError> {
    let mut params_list = vec![];
    let mut peekable = tokens.iter().peekable();
    let err = || ParseError { message: format!("malformed param list: {:?}", tokens) };
    loop {
        let first = peekable.next();
        if first == None {
            break;
        }
        let param_type = parse_type(first.unwrap())?;
        let name = parse_identifier(peekable.next().ok_or(err())?)?;
        params_list.push(Param { param_type, name });
        let next = peekable.next();
        if let Some(&Token::Symbol(ref s)) = next {
            if s == &Symbol::Comma {
                if peekable.peek() == None {
                    return Err(err()); // trailing comma
                }
                continue;
            }
        } else if next == None {
            break;
        }
        return Err(err());
    }
    Ok(params_list)
}

fn parse_var(tokens: &[Token]) -> Result<Var, ParseError> {
    let data_type = parse_type(&tokens[0])?;
    let mut names = vec![];
    for (index, token) in tokens[1..].iter().enumerate() {
        if index % 2 != 0 {
            if token == &Token::Symbol(Symbol::Comma) {
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
            parse_identifier(peekable.by_ref().skip(1).next().ok_or(ParseError {
                message: "malformed subroutine call".to_string(),
            })?)?
        )
    } else {
        // otherwise there is no parent and `first_identifier` is a lone function name
        (None, first_identifier.to_string())
    };

    let mut balance = 0;
    let params_tokens = peekable.by_ref().take_while(|t| {
        match t {
            &&Token::Symbol(Symbol::OpenParen) => balance += 1,
            &&Token::Symbol(Symbol::CloseParen) => balance -= 1,
            _ => (),
        }
        balance != 0
    }).map(|t| t.clone()).collect::<Vec<Token>>();

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
    if next == None {
        return Ok(vec![]);
    }
    let term = match next.unwrap() {
        &Token::IntegerConstant(ref i) => Term::IntegerConstant(*i),
        &Token::StringConstant(ref s) => Term::StringConstant(s.to_string()),
        &Token::Keyword(ref k) => {
            if k == &Keyword::True || k == &Keyword::False || k == &Keyword::Null || k == &Keyword::This {
                Term::KeywordConstant(*k)
            } else {
                return Err(ParseError {
                    message: format!("unexpected keyword `{:?}` in expression", k)
                });
            }
        },
        &Token::Identifier(ref id) => {
            // how to avoid this? the problem was that i need to match on the value of
            // peekable.peek and consume some more tokens within the match arms, but that's
            // borrowing as mutable twice at once per the compiler
            let mut peekable_clone = peekable.clone();
            let next = peekable_clone.peek();

            if next == Some(&&Token::Symbol(Symbol::OpenSquare)) {
                // parse index expr
                let mut balance = 0; // already consumed the open paren
                let index_expr_tokens = peekable.by_ref().take_while(|t| {
                    match t {
                        &&Token::Symbol(Symbol::OpenSquare) => balance += 1,
                        &&Token::Symbol(Symbol::CloseSquare) => balance -= 1,
                        _ => (),
                    }
                    balance != 0
                }).map(|t| t.clone()).collect::<Vec<Token>>();

                Term::IndexExpr(id.to_string(), parse_expression(&index_expr_tokens[1..])?)

            } else if next == Some(&&Token::Symbol(Symbol::OpenParen)) ||
                      next == Some(&&Token::Symbol(Symbol::Period)) {

                // grab everything before the params
                let mut call_tokens = peekable.by_ref()
                    .take_while(|t| t != &&Token::Symbol(Symbol::OpenParen))
                    .map(|t| t.clone()).collect::<Vec<Token>>();

                // grab the params
                let mut paren_balance = 1; // already ate the open paren
                let params_tokens = peekable.by_ref().take_while(|t| {
                    match t {
                        &&Token::Symbol(Symbol::OpenParen) => paren_balance += 1,
                        &&Token::Symbol(Symbol::CloseParen) => paren_balance -= 1,
                        _ => (),
                    }
                    paren_balance != 0
                }).map(|t| t.clone()).collect::<Vec<Token>>();

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
        &Token::Symbol(ref s) => {
            if s == &Symbol::Minus || s == &Symbol::Not {
                // unary op then term
                let op = match s {
                    &Symbol::Minus => UnaryOp::Neg,
                    &Symbol::Not => UnaryOp::Not,
                    _ => panic!("arrrrgh"), // we already checked above! wat
                };
                let rest = parse_expression_inner(
                    &peekable.map(|t| t.clone()).collect::<Vec<Token>>()
                )?;
                let next_term = if let ExpressionItem::Term(ref t) = rest[0] {
                    t.clone()
                } else {
                    return Err(ParseError {
                        message: "expected term after unary op".to_string(),
                    });
                };

                let mut result = vec![
                    ExpressionItem::Term(Term::Unary(op, Box::new(next_term)))
                ];
                // result.extend(&rest[1..]);

                return Ok(result);
            } else if s == &Symbol::OpenParen {
                // parenthetical expression
                let mut balance = 1; // already consumed the open paren
                let parenthetical_tokens = peekable.by_ref().take_while(|t| {
                    match t {
                        &&Token::Symbol(Symbol::OpenParen) => balance += 1,
                        &&Token::Symbol(Symbol::CloseParen) => balance -= 1,
                        _ => (),
                    }
                    balance != 0
                }).map(|t| t.clone()).collect::<Vec<Token>>();
                Term::Parenthetical(parse_expression(&parenthetical_tokens)?)
            } else {
                return Err(ParseError {
                    message: format!("expected term, got symbol `{:?}` in expression", s)
                });
            }
        }
    };

    let mut expressions = vec![ExpressionItem::Term(term)];
    let op = peekable.next();

    if op == None {
        return Ok(expressions); // base case
    } else {
        if let &Token::Symbol(sym) = op.unwrap() {
            let operation: BinaryOp = sym.try_into()?;
            expressions.push(ExpressionItem::Operation(operation));
        } else {
            return Err(ParseError {
                message: "expected an operation".to_string(),
            });
        }
    }
    if peekable.peek() == None {
        Err(ParseError {
            message: "trailing operation in expression, expected a term".to_string(),
        })
    } else {
        expressions.extend(
            parse_expression_inner(&peekable.map(|t| t.clone()).collect::<Vec<Token>>())?
        );
        Ok(expressions)
    }
}

fn parse_let_statement(tokens: &[Token]) -> Result<LetStatement, ParseError> {
    let mut peekable = tokens.iter().peekable();
    let name = parse_identifier(peekable.next().ok_or(ParseError {
        message: "missing identifier in let statement".to_string(),
    })?)?;

    let index_expression = if let Some(&&Token::Symbol(Symbol::OpenSquare)) = peekable.peek() {
        let mut balance = 1; // hop over the open square bracket we already peeked
        let index_expr_tokens = peekable.by_ref().skip(1).take_while(|t| {
            match t {
                &&Token::Symbol(Symbol::OpenSquare) => balance += 1,
                &&Token::Symbol(Symbol::CloseSquare) => balance -= 1,
                _ => (),
            }
            balance != 0
            // todo: is there a way to make this 👇 simpler?
        }).map(|t| t.clone()).collect::<Vec<Token>>();
        Some(parse_expression(&index_expr_tokens)?) // omit open square bracket
    } else {
        None
    };

    // skip over the equals sign
    if peekable.next() != Some(&Token::Symbol(Symbol::Eq)) {
        return Err(ParseError { message: "missing equals sign in let statement".to_string() });
    }

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
    let mut statements = vec![];
    let mut parse_index = 0;
    while parse_index < tokens.len() {
        let begin_token = &tokens[parse_index];
        match begin_token {
            &Token::Keyword(Keyword::Let) => {
                let end_index = parse_index + find_token_index(&tokens[parse_index..], Token::Symbol(Symbol::Semi))?;
                statements.push(
                    Statement::Let(parse_let_statement(&tokens[parse_index + 1..end_index])?)
                );
                parse_index = end_index + 1;
            },
            &Token::Keyword(Keyword::If) => {
                let condition_start = parse_index + 1;
                let condition_end = condition_start + balance_symbol(&tokens[condition_start..], Symbol::OpenParen, Symbol::CloseParen)?;
                let if_body_start = condition_end + 1;
                let if_body_end = if_body_start + balance_symbol(&tokens[if_body_start..], Symbol::OpenCurly, Symbol::CloseCurly)?;
                // check to see if there are any more tokens, and if the next one is `else`
                let (maybe_else_body, end_index) = if if_body_end + 1 < tokens.len() &&
                        &tokens[if_body_end + 1] == &Token::Keyword(Keyword::Else) {

                    let else_body_start = if_body_end + 2; // hop over `else` token to open bracket
                    let else_body_end = else_body_start + balance_symbol(&tokens[else_body_start..], Symbol::OpenCurly, Symbol::CloseCurly)?;
                    (Some(&tokens[else_body_start + 1..else_body_end]), else_body_end)
                } else {
                    (None, if_body_end)
                };
                statements.push(Statement::If(
                    parse_if_statement(
                        &tokens[condition_start + 1..condition_end],
                        &tokens[if_body_start + 1..if_body_end],
                        maybe_else_body
                    )?
                ));
                parse_index = end_index + 1;
            },
            &Token::Keyword(Keyword::While) => {
                // TODO is there a nice way to dedupe this code from above and elsewhere?
                let condition_start = parse_index + 1;
                let condition_end = condition_start + balance_symbol(&tokens[condition_start..], Symbol::OpenParen, Symbol::CloseParen)?;
                let body_start = condition_end + 1;
                let body_end = body_start + balance_symbol(&tokens[body_start..], Symbol::OpenCurly, Symbol::CloseCurly)?;
                statements.push(Statement::While(
                    parse_while_statement(
                        &tokens[condition_start + 1..condition_end],
                        &tokens[body_start + 1..body_end]
                    )?
                ));
                parse_index = body_end + 1;
            },
            &Token::Keyword(Keyword::Do) => {
                let end_index = parse_index + find_token_index(&tokens[parse_index..], Token::Symbol(Symbol::Semi))?;
                let first_identifier = parse_identifier(&tokens[parse_index + 1])?;
                statements.push(Statement::Do(
                    // TODO this should be a subroutine call
                    parse_subroutine_call(&first_identifier, &tokens[parse_index + 2..end_index])?
                ));
                parse_index = end_index + 1;
            },
            &Token::Keyword(Keyword::Return) => {
                let end_index = parse_index + find_token_index(&tokens[parse_index..], Token::Symbol(Symbol::Semi))?;
                statements.push(Statement::Return(
                    parse_expression(&tokens[parse_index + 1..end_index])?
                ));
                parse_index = end_index + 1;
            },
            _ => return Err(ParseError {
                message: format!("unexpected keyword to begin statement: {:?}", begin_token)
            }),
        }
    }
    Ok(statements)
}

fn parse_subroutine_body(body: &[Token]) -> Result<SubroutineBody, ParseError> {
    let mut parse_index = 0;
    let mut var_declarations = vec![];
    while parse_index < body.len() {
        let current_token = &body[parse_index];
        if current_token == &Token::Keyword(Keyword::Var) {
            let declaration_start = parse_index + 1;
            let declaration_end = declaration_start +
                find_token_index(&body[declaration_start..], Token::Symbol(Symbol::Semi))?;
            var_declarations.push(parse_var(&body[declaration_start..declaration_end])?);
            parse_index = declaration_end + 1;
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
    let subroutine_type = match subroutine_type_token {
        &Token::Keyword(Keyword::Method) => SubroutineType::Method,
        &Token::Keyword(Keyword::Function) => SubroutineType::Function,
        &Token::Keyword(Keyword::Constructor) => SubroutineType::Constructor,
        _ => return Err(subroutine_type_err),
    };

    let return_type = if return_type_token == &Token::Keyword(Keyword::Void) {
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
                ParseError { message: "expected return type in subroutine declaration".to_string() }
            )?;
            let name_token = peekable.next().ok_or(
                ParseError { message: "expected name in subroutine declaration".to_string() }
            )?;

            if peekable.next() != Some(&&Token::Symbol(Symbol::OpenParen)) {
                return Err(ParseError {
                    message: "expected `(` to open parameters list".to_string(),
                });
            }
            let params_tokens = peekable.by_ref()
                .take_while(|t| t != &&Token::Symbol(Symbol::CloseParen))
                .map(|t| t.clone()).collect::<Vec<Token>>();

            if peekable.next() != Some(&&Token::Symbol(Symbol::OpenCurly)) {
                return Err(ParseError {
                    message: "expected `{` to open subroutine body".to_string(),
                });
            }

            let mut curly_balance = 1;
            let body_tokens = peekable.by_ref().take_while(|t| {
                match t {
                    &&Token::Symbol(Symbol::OpenCurly) => curly_balance += 1,
                    &&Token::Symbol(Symbol::CloseCurly) => curly_balance -= 1,
                    _ => ()
                }
                curly_balance != 0
            }).map(|t| t.clone()).collect::<Vec<Token>>();

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
    Err(ParseError {
        message: format!("unexpected token in class body: {:?}", tokens[0]),
    })
}

pub fn parse(tokens: &[Token]) -> Result<Class, ParseError> {
    if tokens[0] == Token::Keyword(Keyword::Class) {
        if let Token::Identifier(ref classname) = tokens[1] {
            let body_end = balance_symbol(&tokens[2..], Symbol::OpenCurly, Symbol::CloseCurly)?;
            Ok(Class {
                name: classname.to_string(),
                body: parse_class_body(&tokens[3..body_end + 2])?,
            })
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
            &[
                Token::Keyword(Keyword::Static),
                Token::Keyword(Keyword::Int),
                Token::Identifier("a".to_string()),
                Token::Symbol(Symbol::Comma),
                Token::Identifier("b".to_string()),
                Token::Symbol(Symbol::Semi),
                Token::Keyword(Keyword::Constructor),
            ],
            Token::Symbol(Symbol::Semi),
        ).unwrap(), 5);
    }

    #[test]
    fn test_balance_symbol_simple() {
        assert_eq!(balance_symbol(
            &[
                Token::Symbol(Symbol::OpenCurly),
                Token::Keyword(Keyword::Do),
                Token::Identifier("blargh".to_string()),
                Token::Symbol(Symbol::OpenParen),
                Token::Symbol(Symbol::CloseParen),
                Token::Symbol(Symbol::Semi),
                Token::Symbol(Symbol::CloseCurly),
            ],
            Symbol::OpenCurly,
            Symbol::CloseCurly
        ).unwrap(), 6);
    }

    #[test]
    fn test_balance_symbol_complex() {
        assert_eq!(balance_symbol(
            &[
                Token::Symbol(Symbol::OpenCurly),
                Token::Symbol(Symbol::OpenCurly),
                Token::Keyword(Keyword::Return),
                Token::IntegerConstant(1),
                Token::Symbol(Symbol::Semi),
                Token::Symbol(Symbol::CloseCurly),
                Token::Symbol(Symbol::CloseCurly),
            ],
            Symbol::OpenCurly,
            Symbol::CloseCurly
        ).unwrap(), 6);
    }

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
        assert_eq!(parse_class_var(&input).unwrap(), ClassVar {
            var_type: ClassVarType::Static,
            var: Var {
                data_type: Type::Int,
                names: vec!["foo".to_string()],
            },
        });

        let multiple_declarations = [
            Token::Keyword(Keyword::Field),
            Token::Identifier("MyCustomClass".to_string()),
            Token::Identifier("foo".to_string()),
            Token::Symbol(Symbol::Comma),
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
            vec![Param { param_type: Type::Int, name: "x".to_string() }]
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
                Param { param_type: Type::Int, name: "x".to_string() },
                Param { param_type: Type::Class("Blargh".to_string()), name: "y1".to_string() },
                Param { param_type: Type::Char, name: "y2".to_string() },
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
