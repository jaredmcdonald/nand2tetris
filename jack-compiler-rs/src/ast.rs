use std::fmt;
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

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Statement::*;
        match *self {
            Let(ref s) => write!(f, "<letStatement>\n{}\n</letStatement>", s),
            If(ref s) => write!(f, "<ifStatement>\n{}\n</ifStatement>", s),
            While(ref s) => write!(f, "<whileStatement>\n{}\n</whileStatement>", s),
            Do(ref s) => {
                write!(f,
                    "<doStatement>
                        <keyword>do</keyword>
                        {}<symbol>;</symbol>
                    </doStatement>",
                    s
                )
            },
            Return(ref s) => {
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

impl TryInto<UnaryOp> for Symbol {
    type Error = ParseError;

    fn try_into(self) -> Result<UnaryOp, Self::Error> {
        match self {
            Symbol::Minus => Ok(UnaryOp::Neg),
            Symbol::Not => Ok(UnaryOp::Not),
            _ => Err(Self::Error::new(&format!("unrecognized unary operation `{}`", self))),
        }
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match *self {
            UnaryOp::Neg => Token::Symbol(Symbol::Minus),
            UnaryOp::Not => Token::Symbol(Symbol::Not),
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
        write!(f, "{}", match *self {
            BinaryOp::Plus => Token::Symbol(Symbol::Plus),
            BinaryOp::Minus => Token::Symbol(Symbol::Minus),
            BinaryOp::Mult => Token::Symbol(Symbol::Mult),
            BinaryOp::Div => Token::Symbol(Symbol::Div),
            BinaryOp::And => Token::Symbol(Symbol::Amp),
            BinaryOp::Or => Token::Symbol(Symbol::Pipe),
            BinaryOp::Lt => Token::Symbol(Symbol::Lt),
            BinaryOp::Gt => Token::Symbol(Symbol::Gt),
            BinaryOp::Eq => Token::Symbol(Symbol::Eq),
        })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct SubroutineCall {
    pub parent_name: Option<String>,
    pub subroutine_name: String,
    pub parameters: Vec<Expression>,
}

impl fmt::Display for SubroutineCall {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let parent_prefix = self.parent_name.as_ref().map_or("".to_owned(), |p| {
            format!("<identifier>{}</identifier>\n<symbol>.</symbol>\n", p)
        });

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
            _ => Err(Self::Error::new(&format!("unrecognized operation `{}`", self))),
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
        match *self {
            ExpressionItem::Term(ref t) => write!(f, "{}", t),
            ExpressionItem::Operation(ref o) => write!(f, "{}", o),
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
        use self::Term::*;
        write!(f, "<term>\n{}</term>", match *self {
            IntegerConstant(ref i) => format!("<integerConstant>{}</integerConstant>\n", i),
            StringConstant(ref s) => format!("<stringConstant>{}</stringConstant>\n", s),
            KeywordConstant(ref k) => format!("<keyword>{}</keyword>\n", k),
            VarName(ref v) => format!("<identifier>{}</identifier>\n", v),
            Unary(ref op, ref term) => format!("{}\n{}\n", op, term),
            Parenthetical(ref e) => format!("<symbol>(</symbol>\n{}\n<symbol>)</symbol>\n", e),
            IndexExpr(ref var_name, ref expr) => format!(
                    "<identifier>{}</identifier>
                    <symbol>[</symbol>
                    {}
                    <symbol>]</symbol>\n",
                var_name, expr),
            SubroutineCall(ref s) => format!("{}", s),
        })
    }
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

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let content = self.0.iter().map(|t| format!("{}\n", t)).collect::<String>();
        write!(f, "<expression>\n{}</expression>", content)
    }
}

#[derive(Debug, PartialEq)]
pub struct WhileStatement {
    pub condition: Expression,
    pub body: Vec<Statement>,
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
    pub condition: Expression,
    pub if_body: Vec<Statement>,
    pub else_body: Option<Vec<Statement>>,
}

impl fmt::Display for IfStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let if_body = self.if_body.iter().map(|t| format!("{}", t)).collect::<Vec<_>>().join("\n");
        let maybe_else = self.else_body.as_ref().map_or("".to_owned(), |else_body| {
            format!(
                "\n<keyword>else</keyword>
                <symbol>{{</symbol>
                <statements>
                    {}
                </statements>
                <symbol>}}</symbol>",
                else_body.iter().map(|t| format!("{}", t)).collect::<Vec<_>>().join("\n")
            )
        });
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
    pub name: String,
    pub index_expression: Option<Expression>, // e.g. `let some_array[j + 1] = ...`
    pub expression: Expression,
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let maybe_index_expression = self.index_expression.as_ref().map_or("".to_owned(), |index_expr| {
            format!(
                "\n<symbol>[</symbol>
                    {}
                 <symbol>]</symbol>", index_expr)
        });
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

impl fmt::Display for SubroutineType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<keyword>{}</keyword>", match *self {
            SubroutineType::Constructor => "constructor",
            SubroutineType::Function => "function",
            SubroutineType::Method => "method",
        })
    }
}

#[derive(Debug, PartialEq)]
pub struct SubroutineBody {
    pub var_declarations: Vec<Var>,
    pub statements: Vec<Statement>,
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
    pub subroutine_type: SubroutineType,
    pub return_type: Type,
    pub params: Vec<Var>,
    pub name: String,
    pub body: SubroutineBody,
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

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum VarType {
    Field,
    Static,
    Argument,
    Local,
}

impl fmt::Display for VarType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            VarType::Field => write!(f, "<keyword>field</keyword>"),
            VarType::Static => write!(f, "<keyword>static</keyword>"),
            _ => write!(f, ""),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ClassBodyItem {
    ClassVar(Var),
    Subroutine(Subroutine),
}

impl fmt::Display for ClassBodyItem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ClassBodyItem::ClassVar(ref cv) => write!(f, "<classVarDec>\n{}\n<symbol>;</symbol>\n</classVarDec>", cv),
            ClassBodyItem::Subroutine(ref sr) => write!(f, "{}", sr),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Void,
    Int,
    Char,
    Boolean,
    Class(String),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::Type::*;
        match *self {
            Void => write!(f, "<keyword>void</keyword>"),
            Int => write!(f, "<keyword>int</keyword>"),
            Char => write!(f, "<keyword>char</keyword>"),
            Boolean => write!(f, "<keyword>boolean</keyword>"),
            Class(ref s) => write!(f, "<identifier>{}</identifier>", s),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Var {
    pub var_type: VarType,
    pub data_type: Type,
    pub names: Vec<String>, // can declare more than one at once, e.g. `static int x, y;`
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let formatted_var_type = format!("{}", self.var_type);
        let names = self.names.iter().map(|n| format!("<identifier>{}</identifier>", n))
            .collect::<Vec<String>>()
            .join("\n<symbol>,</symbol>\n");
        write!(f,
            "{}{}\n{}",
            if formatted_var_type == "" { "".to_owned() } else { format!("{}\n", formatted_var_type) },
            self.data_type,
            names
        )
    }
}

#[derive(Debug, PartialEq)]
pub struct Class {
    pub name: String,
    pub body: Vec<ClassBodyItem>,
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
