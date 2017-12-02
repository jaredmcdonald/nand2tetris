use std::collections::HashMap;
use regex::{Regex, self};

pub enum Token {
    Keyword(Keyword),
    Unknown(String),
}

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Symbol {
    OpenCurly,
    CloseCurly,
    OpenParen,
    CloseParen,
    OpenSquare,
    CloseSquare,
    Dot,
    Comma,
    Semicolon,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Ampersand,
    Pipe,
    Lt,
    Gt,
    Equals,
    Tilde,
}

fn generate_keyword_map() -> HashMap<String, Keyword> {
    let mut keywords = HashMap::new();
    keywords.insert("class".to_string(), Keyword::Class);
    keywords.insert("constructor".to_string(), Keyword::Constructor);
    keywords.insert("function".to_string(), Keyword::Function);
    keywords.insert("method".to_string(), Keyword::Method);
    keywords.insert("field".to_string(), Keyword::Field);
    keywords.insert("static".to_string(), Keyword::Static);
    keywords.insert("var".to_string(), Keyword::Var);
    keywords.insert("int".to_string(), Keyword::Int);
    keywords.insert("char".to_string(), Keyword::Char);
    keywords.insert("boolean".to_string(), Keyword::Boolean);
    keywords.insert("void".to_string(), Keyword::Void);
    keywords.insert("true".to_string(), Keyword::True);
    keywords.insert("false".to_string(), Keyword::False);
    keywords.insert("null".to_string(), Keyword::Null);
    keywords.insert("this".to_string(), Keyword::This);
    keywords.insert("let".to_string(), Keyword::Let);
    keywords.insert("do".to_string(), Keyword::Do);
    keywords.insert("if".to_string(), Keyword::If);
    keywords.insert("else".to_string(), Keyword::Else);
    keywords.insert("while".to_string(), Keyword::While);
    keywords.insert("return".to_string(), Keyword::Return);
    keywords
}

fn generate_symbol_map() -> HashMap<String, Symbol> {
    let mut symbols = HashMap::new();
    symbols.insert("\\{".to_string(), Symbol::OpenCurly);
    symbols.insert("\\}".to_string(), Symbol::CloseCurly);
    symbols.insert("\\(".to_string(), Symbol::OpenParen);
    symbols.insert("\\)".to_string(), Symbol::CloseParen);
    symbols.insert("\\[".to_string(), Symbol::OpenSquare);
    symbols.insert("\\]".to_string(), Symbol::CloseSquare);
    symbols.insert("\\.".to_string(), Symbol::Dot);
    symbols.insert(",".to_string(), Symbol::Comma);
    symbols.insert(";".to_string(), Symbol::Semicolon);
    symbols.insert("\\+".to_string(), Symbol::Plus);
    symbols.insert("\\-".to_string(), Symbol::Minus);
    symbols.insert("\\*".to_string(), Symbol::Asterisk);
    symbols.insert("/".to_string(), Symbol::Slash);
    symbols.insert("&".to_string(), Symbol::Ampersand);
    symbols.insert("\\|".to_string(), Symbol::Pipe);
    symbols.insert("<".to_string(), Symbol::Lt);
    symbols.insert(">".to_string(), Symbol::Gt);
    symbols.insert("=".to_string(), Symbol::Equals);
    symbols.insert("~".to_string(), Symbol::Tilde);
    symbols
}

fn tokenize(input: &str) -> Result<Vec<Token>, regex::Error> {
    let keywords = generate_keyword_map();
    let symbols = generate_symbol_map();
    let keywords_re = keywords.keys().map(|s| s.to_string()).collect::<Vec<String>>().join("|");
    let symbols_re = symbols.keys().map(|s| s.to_string()).collect::<Vec<String>>().join("|");
    let identifier_re = "[A-Za-z0-9_]+"; // todo how to invalidate leading digits in the regex?
    let int_re = "[0-9]+";
    let string_re = "\"[^\"]*\"";
    let tokenize_regex = Regex::new(format!(r"({}|{}|{}|{}|{})", keywords_re, symbols_re, int_re, string_re, identifier_re).as_ref())?;

    for mat in tokenize_regex.find_iter(input) {
        println!("{}", mat.as_str());
    }

    Ok(vec![]) // todo
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_tokenize() {
        let tokenized = tokenize("class Foo { function blargh1() { return \"blargh\"; }}").unwrap();
    }
}
