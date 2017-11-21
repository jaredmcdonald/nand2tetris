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
pub enum Instruction {
    Push(Push),
    Pop(Pop),
    Comparison(Comparison),
    Unary(Unary),
    Binary(Binary),
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
pub struct Push {
    pub segment: MemorySegment,
    pub index: u16,
}

#[derive(Debug, PartialEq)]
pub struct Pop {
    pub segment: MemorySegment,
    pub index: u16,
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

pub fn parse(lines: &[String]) -> Vec<Instruction> {
    lines.iter().filter_map(|line| {
        if let Some(l) = strip_comments_and_whitespace(line) {
            Some(parse_line(&l))
        } else { None }
    }).collect()
}

fn to_memory_segment(name: &str) -> MemorySegment {
    match name {
        "constant" => MemorySegment::Constant,
        "local" => MemorySegment::Local,
        "argument" => MemorySegment::Argument,
        "this" => MemorySegment::This,
        "that" => MemorySegment::That,
        "pointer" => MemorySegment::Pointer,
        "temp" => MemorySegment::Temp,
        "static" => MemorySegment::Static,
        _ => panic!("unrecognized memory segment: {}", name),
    }
}

// assumes the line has already been stripped of whitespace and comments
fn parse_line(line: &str) -> Instruction {
    let space_split = line.split(" ").collect::<Vec<&str>>();
    match space_split[0] {
        "push" => Instruction::Push(Push {
            segment: to_memory_segment(space_split[1]),
            index: space_split[2].parse::<u16>().unwrap(),
        }),
        "pop" => Instruction::Pop(Pop {
            segment: to_memory_segment(space_split[1]),
            index: space_split[2].parse::<u16>().unwrap(),
        }),
        "eq" => Instruction::Comparison(Comparison::Eq),
        "gt" => Instruction::Comparison(Comparison::Gt),
        "lt" => Instruction::Comparison(Comparison::Lt),
        "add" => Instruction::Binary(Binary::Add),
        "sub" => Instruction::Binary(Binary::Sub),
        "and" => Instruction::Binary(Binary::And),
        "or" => Instruction::Binary(Binary::Or),
        "neg" => Instruction::Unary(Unary::Neg),
        "not" => Instruction::Unary(Unary::Not),
        _ => panic!("unknown instruction in line: {}", line),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_push() {
        assert_eq!(
            parse_line("push constant 15"),
            Instruction::Push(Push { segment: MemorySegment::Constant, index: 15 })
        );
    }

    #[test]
    fn test_parse_push_non_constant() {
        assert_eq!(
            parse_line("push local 10"),
            Instruction::Push(Push { segment: MemorySegment::Local, index: 10 })
        );
    }

    #[test]
    fn test_parse_others() {
        assert_eq!(parse_line("add"), Instruction::Binary(Binary::Add));
        assert_eq!(parse_line("sub"), Instruction::Binary(Binary::Sub));
        assert_eq!(parse_line("and"), Instruction::Binary(Binary::And));
        assert_eq!(parse_line("or"), Instruction::Binary(Binary::Or));
        assert_eq!(parse_line("neg"), Instruction::Unary(Unary::Neg));
        assert_eq!(parse_line("not"), Instruction::Unary(Unary::Not));
        assert_eq!(parse_line("eq"), Instruction::Comparison(Comparison::Eq));
        assert_eq!(parse_line("gt"), Instruction::Comparison(Comparison::Gt));
        assert_eq!(parse_line("lt"), Instruction::Comparison(Comparison::Lt));
    }

    #[test]
    fn test_parse_comments_whitespace() {
        let test_lines = vec!["// Hello".to_string(), " push constant 12 // Hi".to_string(), " ".to_string()];
        assert_eq!(parse(test_lines.as_slice()).len(), 1);
    }
}
