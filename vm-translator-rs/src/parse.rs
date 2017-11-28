#[derive(Debug, PartialEq)]
pub enum Comparison {
    Eq,
    Gt,
    Lt,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Unary {
    Neg,
    Not,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Binary {
    Add,
    Sub,
    And,
    Or,
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: String,
    pub local_count: u16,
}

#[derive(Debug, PartialEq)]
pub struct FunctionCall {
    pub name: String,
    pub arg_count: u16,
}

// is there a better way of subsetting enums? probably
#[derive(Debug, PartialEq)]
pub enum Instruction {
    Push(MemoryLocation),
    Pop(MemoryLocation),
    Comparison(Comparison),
    Unary(Unary),
    Binary(Binary),
    Label(String),
    Goto(String),
    IfGoto(String),
    Function(Function),
    Call(FunctionCall),
    Return,
}

#[derive(Debug, PartialEq)]
pub enum MemorySegment {
    Constant,
    Local,
    Argument,
    This,
    That,
    Pointer,
    Temp,
    Static,
}

#[derive(Debug, PartialEq)]
pub struct MemoryLocation {
    pub segment: MemorySegment,
    pub index: u16,
}

#[derive(Debug, PartialEq)]
pub enum ParseErrorKind {
    UnrecognizedMemorySegment,
    ParseIntError,
    ArityMismatch,
    UnrecognizedInstruction,
}

#[derive(Debug, PartialEq)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub source: String,
}

fn strip_comments_and_whitespace(line: &str) -> Option<String> {
    let without_comment = line.split("//").collect::<Vec<&str>>(); // remove everything after '//'
    let trimmed = without_comment[0].trim();
    if trimmed.len() == 0 {
        None
    } else {
        Some(trimmed.to_string())
    }
}

pub fn parse(lines: &[String]) -> Result<Vec<Instruction>, ParseError> {
    lines.iter().filter_map(|line| {
        if let Some(l) = strip_comments_and_whitespace(line) {
            Some(parse_line(&l))
        } else { None }
    }).collect::<Result<Vec<Instruction>, _>>()
}

fn to_memory_segment(name: &str, context: &str) -> Result<MemorySegment, ParseError> {
    match name {
        "constant" => Ok(MemorySegment::Constant),
        "local" => Ok(MemorySegment::Local),
        "argument" => Ok(MemorySegment::Argument),
        "this" => Ok(MemorySegment::This),
        "that" => Ok(MemorySegment::That),
        "pointer" => Ok(MemorySegment::Pointer),
        "temp" => Ok(MemorySegment::Temp),
        "static" => Ok(MemorySegment::Static),
        _ => Err(ParseError { kind: ParseErrorKind::UnrecognizedMemorySegment, source: context.to_string() }),
    }
}

fn parse_number(i: &str, context: &str) -> Result<u16, ParseError> {
    match i.parse::<u16>() {
        Ok(result) => Ok(result),
        _ => Err(ParseError {
            kind: ParseErrorKind::ParseIntError,
            source: context.to_string(),
        }),
    }
}

fn incorrect_arity(context: &str) -> ParseError {
    ParseError {
        kind: ParseErrorKind::ArityMismatch,
        source: context.to_string(),
    }
}

// assumes the line has already been stripped of whitespace and comments
fn parse_line(line: &str) -> Result<Instruction, ParseError> {
    let space_split = line.split(" ").collect::<Vec<&str>>();
    let len = space_split.len();
    match space_split[0] {
        "push" => {
            if len == 3 { // probably a nicer way to do all the arity checking, hmmmm
                Ok(Instruction::Push(MemoryLocation {
                    segment: to_memory_segment(space_split[1], line)?,
                    index: parse_number(space_split[2], line)?,
                }))
            } else { Err(incorrect_arity(line)) }
        },
        "pop" => {
            if len == 3 {
                Ok(Instruction::Pop(MemoryLocation {
                    segment: to_memory_segment(space_split[1], line)?,
                    index: parse_number(space_split[2], line)?,
                }))
            } else { Err(incorrect_arity(line)) }
        },
        "eq" => Ok(Instruction::Comparison(Comparison::Eq)),
        "gt" => Ok(Instruction::Comparison(Comparison::Gt)),
        "lt" => Ok(Instruction::Comparison(Comparison::Lt)),
        "add" => Ok(Instruction::Binary(Binary::Add)),
        "sub" => Ok(Instruction::Binary(Binary::Sub)),
        "and" => Ok(Instruction::Binary(Binary::And)),
        "or" => Ok(Instruction::Binary(Binary::Or)),
        "neg" => Ok(Instruction::Unary(Unary::Neg)),
        "not" => Ok(Instruction::Unary(Unary::Not)),
        "label" => {
            if len == 2 {
                Ok(Instruction::Label(space_split[1].to_string()))
            } else { Err(incorrect_arity(line)) }
        },
        "goto" => {
            if len == 2 {
                Ok(Instruction::Goto(space_split[1].to_string()))
            } else { Err(incorrect_arity(line)) }
        },
        "if-goto" => {
            if len == 2 {
                Ok(Instruction::IfGoto(space_split[1].to_string()))
            } else { Err(incorrect_arity(line)) }
        },
        "function" => {
            if len == 3 {
                Ok(Instruction::Function(Function {
                    name: space_split[1].to_string(),
                    local_count: parse_number(space_split[2], line)?,
                }))
            } else { Err(incorrect_arity(line)) }
        },
        "call" => {
            if len == 3 {
                Ok(Instruction::Call(FunctionCall {
                    name: space_split[1].to_string(),
                    arg_count: parse_number(space_split[2], line)?,
                }))
            } else { Err(incorrect_arity(line)) }
        },
        "return" => Ok(Instruction::Return),
        _ => Err(ParseError { kind: ParseErrorKind::UnrecognizedInstruction, source: line.to_string() }),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_push() {
        assert_eq!(
            parse_line("push constant 15").unwrap(),
            Instruction::Push(MemoryLocation { segment: MemorySegment::Constant, index: 15 })
        );
    }

    #[test]
    fn test_parse_pop() {
        assert_eq!(
            parse_line("pop static 15").unwrap(),
            Instruction::Pop(MemoryLocation { segment: MemorySegment::Static, index: 15 })
        );
    }

    #[test]
    fn test_parse_push_non_constant() {
        assert_eq!(
            parse_line("push local 10").unwrap(),
            Instruction::Push(MemoryLocation { segment: MemorySegment::Local, index: 10 })
        );
    }

    #[test]
    fn test_parse_others() {
        assert_eq!(parse_line("add").unwrap(), Instruction::Binary(Binary::Add));
        assert_eq!(parse_line("sub").unwrap(), Instruction::Binary(Binary::Sub));
        assert_eq!(parse_line("and").unwrap(), Instruction::Binary(Binary::And));
        assert_eq!(parse_line("or").unwrap(), Instruction::Binary(Binary::Or));
        assert_eq!(parse_line("neg").unwrap(), Instruction::Unary(Unary::Neg));
        assert_eq!(parse_line("not").unwrap(), Instruction::Unary(Unary::Not));
        assert_eq!(parse_line("eq").unwrap(), Instruction::Comparison(Comparison::Eq));
        assert_eq!(parse_line("gt").unwrap(), Instruction::Comparison(Comparison::Gt));
        assert_eq!(parse_line("lt").unwrap(), Instruction::Comparison(Comparison::Lt));
    }

    #[test]
    fn test_parse_comments_whitespace() {
        let test_lines = vec!["// Hello".to_string(), " push constant 12 // Hi".to_string(), " ".to_string()];
        assert_eq!(parse(test_lines.as_slice()).unwrap().len(), 1);
    }

    #[test]
    fn test_bail_on_first_error() {
        // eventually it would be nice to get both errors
        let test_lines = vec!["push constant".to_string(), "argh blargh".to_string()];
        assert_eq!(
            parse(test_lines.as_slice()),
            Err(ParseError {
                kind: ParseErrorKind::ArityMismatch,
                source: "push constant".to_string(),
            })
        );
    }

    #[test]
    fn test_parse_control_flow() {
        assert_eq!(parse_line("label foobar").unwrap(), Instruction::Label("foobar".to_string()));
        assert_eq!(parse_line("goto blargh.argh").unwrap(), Instruction::Goto("blargh.argh".to_string()));
        assert_eq!(parse_line("if-goto abcdefg:hijklmnop").unwrap(), Instruction::IfGoto("abcdefg:hijklmnop".to_string()));
    }

    #[test]
    fn test_parse_function_return() {
        assert_eq!(parse_line("function Sys.init 0").unwrap(), Instruction::Function(Function {
            name: "Sys.init".to_string(),
            local_count: 0,
        }));
        assert_eq!(parse_line("return").unwrap(), Instruction::Return);
    }

    #[test]
    fn test_parse_function_call() {
        assert_eq!(parse_line("call Arg.blarg 10").unwrap(), Instruction::Call(FunctionCall {
            name: "Arg.blarg".to_string(),
            arg_count: 10,
        }));
    }

    #[test]
    fn test_parse_errors() {
        assert_eq!(
            parse_line("something-unimplemented 123"),
            Err(ParseError {
                kind: ParseErrorKind::UnrecognizedInstruction,
                source: "something-unimplemented 123".to_string()
            })
        );
        assert_eq!(
            parse_line("push constant argh"),
            Err(ParseError {
                kind: ParseErrorKind::ParseIntError,
                source: "push constant argh".to_string(),
            })
        );
        assert_eq!(
            parse_line("push blargh 1"),
            Err(ParseError {
                kind: ParseErrorKind::UnrecognizedMemorySegment,
                source: "push blargh 1".to_string(),
            })
        );
        assert_eq!(
            parse_line("push constant"),
            Err(ParseError {
                kind: ParseErrorKind::ArityMismatch,
                source: "push constant".to_string(),
            })
        );
        assert_eq!(
            parse_line("goto"),
            Err(ParseError {
                kind: ParseErrorKind::ArityMismatch,
                source: "goto".to_string(),
            })
        );
    }
}
