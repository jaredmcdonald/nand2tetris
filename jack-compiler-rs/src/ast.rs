use std::convert::TryInto;
use tokenize::{Token, Symbol, Keyword};

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(LetStatement),
    If(IfStatement),
    While(WhileStatement),
    Do(SubroutineCall),
    Return(Expression),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOp {
    Neg,
    Not,
}

impl TryInto<UnaryOp> for Symbol {
    type Error = ParseError;

    fn try_into(self) -> Result<UnaryOp, Self::Error> {
        match self {
            Symbol::Minus => Ok(UnaryOp::Neg),
            Symbol::Not => Ok(UnaryOp::Not),
            _ => Err(Self::Error::new(&format!("unrecognized unary operation `{:?}`", self))),
        }
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

#[derive(Debug, PartialEq, Clone)]
pub struct SubroutineCall {
    pub parent_name: Option<String>,
    pub subroutine_name: String,
    pub parameters: Vec<Expression>,
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
            _ => Err(Self::Error::new(&format!("unrecognized operation `{:?}`", self))),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ExpressionItem {
    Term(Term),
    Operation(BinaryOp),
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

#[derive(Debug, PartialEq, Clone)]
pub struct Expression(pub Vec<ExpressionItem>);

// cf https://doc.rust-lang.org/std/iter/trait.IntoIterator.html#examples
impl IntoIterator for Expression {
    type Item = ExpressionItem;
    type IntoIter = ::std::vec::IntoIter<ExpressionItem>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl Expression {
    pub fn len(&self) -> usize {
        self.0.len()
    }
}

#[derive(Debug, PartialEq)]
pub struct WhileStatement {
    pub condition: Expression,
    pub body: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub struct IfStatement {
    pub condition: Expression,
    pub if_body: Vec<Statement>,
    pub else_body: Option<Vec<Statement>>,
}

#[derive(Debug, PartialEq)]
pub struct LetStatement {
    pub name: String,
    pub index_expression: Option<Expression>, // e.g. `let some_array[j + 1] = ...`
    pub expression: Expression,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum SubroutineType {
    Constructor,
    Function,
    Method,
}

impl TryInto<SubroutineType> for Keyword {
    type Error = ParseError;

    fn try_into(self) -> Result<SubroutineType, Self::Error> {
        match self {
            Keyword::Method => Ok(SubroutineType::Method),
            Keyword::Function => Ok(SubroutineType::Function),
            Keyword::Constructor => Ok(SubroutineType::Constructor),
            _ => Err(Self::Error::new(&format!("expected `constructor`, `function` or `method`, got {:?}", self))),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct SubroutineBody {
    pub var_declarations: Vec<Var>,
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub struct Subroutine {
    pub subroutine_type: SubroutineType,
    pub return_type: Type,
    pub params: Vec<Var>,
    pub name: String,
    pub body: SubroutineBody,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum VarType {
    Field,
    Static,
    Argument,
    Local,
}

#[derive(Debug, PartialEq)]
pub enum ClassBodyItem {
    ClassVar(Var),
    Subroutine(Subroutine),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Void,
    Int,
    Char,
    Boolean,
    Class(String),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Var {
    pub var_type: VarType,
    pub data_type: Type,
    pub names: Vec<String>, // can declare more than one at once, e.g. `static int x, y;`
}

#[derive(Debug, PartialEq)]
pub struct Class {
    pub name: String,
    pub body: Vec<ClassBodyItem>,
}

#[derive(Debug, PartialEq)]
pub struct ParseError {
    pub message: String,
}

impl ParseError {
    pub fn new(msg: &str) -> ParseError {
        ParseError { message: msg.to_owned() }
    }
}

impl TryInto<Type> for Token {
    type Error = ParseError;

    fn try_into(self) -> Result<Type, Self::Error> {
        match self {
            Token::Keyword(Keyword::Void) => Ok(Type::Void),
            Token::Keyword(Keyword::Int) => Ok(Type::Int),
            Token::Keyword(Keyword::Char) => Ok(Type::Char),
            Token::Keyword(Keyword::Boolean) => Ok(Type::Boolean),
            Token::Identifier(ref id) => Ok(Type::Class(id.to_string())),
            _ => Err(Self::Error::new(&format!("expected a type, got token {:?}", self))),
        }
    }
}
