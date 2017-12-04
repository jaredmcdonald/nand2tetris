use std::convert::TryInto;
use std::fmt;
use std::num::ParseIntError;
use std::collections::HashMap;
use regex::{Regex, Captures, self};

#[derive(Debug, PartialEq)]
pub enum TokenError {
    KeywordError(KeywordError),
    SymbolError(SymbolError),
    InvalidTokenError(String),
    InvalidIdentifierError(String),
    IntTooBigError(u16),
    RegexError(regex::Error),
    ParseIntError(ParseIntError),
    Unknown,
}

#[derive(Debug, PartialEq, Hash, Eq, Clone, Copy)]
pub enum Keyword {
    Class,
    Constructor,
    Function,
    Method,
    Field,
    Static,
    Var,
    Int,
    Char,
    Boolean,
    Void,
    True,
    False,
    Null,
    This,
    Let,
    Do,
    If,
    Else,
    While,
    Return,
}

#[derive(Debug, PartialEq, Hash, Eq, Clone, Copy)]
pub enum Symbol {
    OpenSquare,
    CloseSquare,
    OpenCurly,
    CloseCurly,
    OpenParen,
    CloseParen,
    Period,
    Comma,
    Semi,
    Plus,
    Minus,
    Mult,
    Div,
    Amp,
    Pipe,
    Lt,
    Gt,
    Eq,
    Not,
}

#[derive(Debug, PartialEq)]
pub struct KeywordError {
    message: String,
}

impl TryInto<Keyword> for String {
    type Error = KeywordError;

    fn try_into(self) -> Result<Keyword, Self::Error> {
        if let Some(kw) = STRING_TO_KEYWORD.get(&self) {
            Ok(*kw)
        } else {
            Err(Self::Error { message: format!("unrecognized keyword `{}`", self) })
        }
    }
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", KEYWORD_TO_STRING.get(self).unwrap())
    }
}

#[derive(Debug, PartialEq)]
pub struct SymbolError {
    message: String,
}

impl TryInto<Symbol> for String {
    type Error = SymbolError;

    fn try_into(self) -> Result<Symbol, Self::Error> {
        if let Some(sym) = STRING_TO_SYMBOL.get(&self) {
            Ok(*sym)
        } else {
            Err(Self::Error { message: format!("unrecognized symbol `{}`", self) })
        }
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", SYMBOL_TO_STRING.get(self).unwrap())
    }
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Keyword(Keyword),
    Symbol(Symbol),
    Identifier(String),
    StringConstant(String),
    IntegerConstant(String),
}

lazy_static! {
    static ref STRING_TO_KEYWORD: HashMap<String, Keyword> = {
        let mut map = HashMap::new();
        for &(s, k) in get_keyword_pairs().iter() {
            map.insert(s.to_string(), k);
        }
        map
    };

    static ref KEYWORD_TO_STRING: HashMap<Keyword, String> = {
        let mut map = HashMap::new();
        for &(s, k) in get_keyword_pairs().iter() {
            map.insert(k, s.to_string());
        }
        map
    };

    static ref STRING_TO_SYMBOL: HashMap<String, Symbol> = {
        let mut map = HashMap::new();
        for &(string, sym) in get_symbol_pairs().iter() {
            map.insert(string.to_string(), sym);
        }
        map
    };

    static ref SYMBOL_TO_STRING: HashMap<Symbol, String> = {
        let mut map = HashMap::new();
        for &(string, sym) in get_symbol_pairs().iter() {
            map.insert(sym, string.to_string());
        }
        map
    };
}

const fn get_symbol_pairs() -> [(&'static str, Symbol); 19] {
    [
        ("[", Symbol::OpenSquare),
        ("]", Symbol::CloseSquare),
        ("{", Symbol::OpenCurly),
        ("}", Symbol::CloseCurly),
        ("(", Symbol::OpenParen),
        (")", Symbol::CloseParen),
        (".", Symbol::Period),
        (",", Symbol::Comma),
        (";", Symbol::Semi),
        ("+", Symbol::Plus),
        ("-", Symbol::Minus),
        ("*", Symbol::Mult),
        ("/", Symbol::Div),
        ("&", Symbol::Amp),
        ("|", Symbol::Pipe),
        ("<", Symbol::Lt),
        (">", Symbol::Gt),
        ("=", Symbol::Eq),
        ("~", Symbol::Not),
    ]
}

const fn get_keyword_pairs() -> [(&'static str, Keyword); 21] {
    [
        ("class", Keyword::Class),
        ("constructor", Keyword::Constructor),
        ("function", Keyword::Function),
        ("method", Keyword::Method),
        ("field", Keyword::Field),
        ("static", Keyword::Static),
        ("var", Keyword::Var),
        ("int", Keyword::Int),
        ("char", Keyword::Char),
        ("boolean", Keyword::Boolean),
        ("void", Keyword::Void),
        ("true", Keyword::True),
        ("false", Keyword::False),
        ("null", Keyword::Null),
        ("this", Keyword::This),
        ("let", Keyword::Let),
        ("do", Keyword::Do),
        ("if", Keyword::If),
        ("else", Keyword::Else),
        ("while", Keyword::While),
        ("return", Keyword::Return),
    ]
}

// XML formatting for the book's tokenization test cases
impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let (label, value) = match self {
            &Token::Keyword(ref v) => ("keyword", v.to_string()),
            &Token::Symbol(ref v) => ("symbol", {
                // need to escape some symbols for xml 🙄
                if v == &Symbol::Lt {
                    "&lt;".to_string()
                } else if v == &Symbol::Gt {
                    "&gt;".to_string()
                } else if v == &Symbol::Amp {
                    "&amp;".to_string()
                } else {
                    v.to_string()
                }
            }),
            &Token::Identifier(ref v) => ("identifier", v.to_string()),
            &Token::StringConstant(ref v) => ("stringConstant", v.to_string()),
            &Token::IntegerConstant(ref v) => ("integerConstant", v.to_string()),
        };
        write!(f, "<{0}>{1}</{0}>", label, value)
    }
}

impl From<regex::Error> for TokenError {
    fn from(error: regex::Error) -> Self {
        TokenError::RegexError(error)
    }
}

impl From<KeywordError> for TokenError {
    fn from(error: KeywordError) -> Self {
        TokenError::KeywordError(error)
    }
}

impl From<SymbolError> for TokenError {
    fn from(error: SymbolError) -> Self {
        TokenError::SymbolError(error)
    }
}

impl From<ParseIntError> for TokenError {
    fn from(error: ParseIntError) -> Self {
        TokenError::ParseIntError(error)
    }
}

fn strip_comments(input: &str) -> Result<String, regex::Error> {
    // match string first, then comments, to make sure we don't
    // remove pieces of strings that look like comments
    // h/t https://stackoverflow.com/questions/2319019/using-regex-to-remove-comments-from-source-files/18381470#18381470
    let comment_regex = Regex::new("(?xs) # `s` flag tells `.` to match newlines
        (?P<string>\"[^\"]*\")| # string
        (/\\*.*?\\*/)|          # multiline comment
        (//[^\\n]*)             # line comment
    ")?;

    // https://doc.rust-lang.org/regex/regex/struct.Regex.html#method.replace
    Ok(comment_regex.replace_all(input, |caps: &Captures| {
        if let Some(mat) = caps.name("string") {
            mat.as_str().to_string() // in a string, don't replace
        } else {
            "".to_string()           // comment! replace with empty string
        }
    }).to_string())
}

pub fn tokenize(input: &str) -> Result<Vec<Token>, TokenError> {
    let tokenize_regex = Regex::new("(?x) # <-- ignore whitespace and comments (beginning w/ '#')
        (?P<keyword>class|constructor|function|method|field|static|var|int|char|boolean|void|true|
                    false|null|this|let|do|if|else|while|return)|
        (?P<symbol>[\\{\\}}\\(\\)\\[\\]\\.,;\\+\\-\\*/&\\|<>=~])|
        \"(?P<string>[^\"]*)\"|
        (?P<identifier>[[:alpha:]_]{1}[[:alnum:]_]*)|
        (?P<invalid_identifier>[[:digit:]]+[[:alpha:]_]+[[:alnum:]_]*)|
        (?P<integer>[[:digit:]]+)|
        (?P<unknown>[^\\s]+)",
    )?;

    let comment_stripped = strip_comments(input)?;

    let tokenized = tokenize_regex.captures_iter(&comment_stripped).map(|capture| {
        if let Some(mat) = capture.name("keyword") {
            Ok(Token::Keyword(mat.as_str().to_string().try_into()?))
        } else if let Some(mat) = capture.name("symbol") {
            Ok(Token::Symbol(mat.as_str().to_string().try_into()?))
        } else if let Some(mat) = capture.name("string") {
            Ok(Token::StringConstant(mat.as_str().to_string()))
        } else if let Some(mat) = capture.name("identifier") {
            Ok(Token::Identifier(mat.as_str().to_string()))
        } else if let Some(mat) = capture.name("integer") {
            // attempt to parse it into a u16; if it doesn't parse, it's likely too big
            let parsed = mat.as_str().parse::<u16>()?;
            // it actually has to fit into 15 bits per our specification
            if parsed < 0b111111111111111 {
                // it's fine, cajole back to a string
                Ok(Token::IntegerConstant(format!("{}", parsed)))
            } else {
                Err(TokenError::IntTooBigError(parsed))
            }
        } else if let Some(mat) = capture.name("invalid_identifier") {
            Err(TokenError::InvalidIdentifierError(mat.as_str().to_string()))
        } else if let Some(mat) = capture.name("unknown") {
            Err(TokenError::InvalidTokenError(mat.as_str().to_string()))
        } else {
            Err(TokenError::Unknown)
        }
    }).collect::<Result<Vec<Token>, TokenError>>()?;

    Ok(tokenized)
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_strip_comments() {
        assert_eq!(strip_comments("hi // blargh\n").unwrap(), "hi \n");
        assert_eq!(strip_comments("hi /* blargh\n */ hi").unwrap(), "hi  hi");
        assert_eq!(strip_comments("\"/* hi */\"").unwrap(), "\"/* hi */\"");
        assert_eq!(strip_comments("/* // hi */hey").unwrap(), "hey");
        assert_eq!(strip_comments("/* \"this is a comment\" */\"but this is not\"").unwrap(), "\"but this is not\"");
        assert_eq!(strip_comments("/*hi*/Hello/*hi again*///sup\n").unwrap(), "Hello\n")
    }

    #[test]
    fn test_tokenize() {
        assert_eq!(
            tokenize("class Foo { function void blargh1() { return \"blargh\"; }}").unwrap().len(),
            14
        );

        assert_eq!(tokenize("var int i, j;").unwrap().len(), 6);
    }

    #[test]
    fn test_tokenize_string_precedence() {
        assert_eq!(tokenize("\"int\"").unwrap()[0], Token::StringConstant("int".to_string()));
        assert_eq!(tokenize("\"{}\"").unwrap()[0], Token::StringConstant("{}".to_string()));
    }

    #[test]
    fn test_tokenize_errors() {
        match tokenize("let i = 3 % 5;") {
            Err(e) => assert_eq!(e, TokenError::InvalidTokenError("%".to_string())),
            _ => panic!("should have been an err"),
        };

        match tokenize("let i = 65536;") {
            Err(TokenError::ParseIntError(_)) => assert!(true),
            _ => panic!("should have been a ParseIntError"),
        };

        match tokenize("let i = 45000;") {
            Err(TokenError::IntTooBigError(i)) => assert_eq!(i, 45000),
            _ => panic!("should have been an err"),
        };

        match tokenize("let 11_blargh = 2;") {
            Err(TokenError::InvalidIdentifierError(id)) => assert_eq!(id, "11_blargh"),
            _ => panic!("should have been an err"),
        }
    }

    #[test]
    fn test_tokenize_with_comments() {
        let tokenized = tokenize("/**\n* hi */class Foo { // a class
            function void blargh1() { /* yeah \"lol\" */ return \"blargh\"; }}").unwrap();
        assert_eq!(tokenized.len(), 14);
    }
}
