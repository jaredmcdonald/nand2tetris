#[derive(Debug, PartialEq)]
pub enum Instruction {
    Push(Push),
    Pop(Pop),
    Add,
    Sub,
    Neg,
    Eq,
    Gt,
    Lt,
    And,
    Or,
    Not,
    Unknown,
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
    Unknown,
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

pub fn parse(lines: &[String]) -> Vec<Instruction> {
    lines.iter().map(|l| parse_line(l)).collect()
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
        _ => MemorySegment::Unknown,
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
        "add" => Instruction::Add,
        "sub" => Instruction::Sub,
        "neg" => Instruction::Neg,
        "eq" => Instruction::Eq,
        "gt" => Instruction::Gt,
        "lt" => Instruction::Lt,
        "and" => Instruction::And,
        "or" => Instruction::Or,
        "not" => Instruction::Not,
        _ => Instruction::Unknown,
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
        assert_eq!(parse_line("add"), Instruction::Add);
        assert_eq!(parse_line("sub"), Instruction::Sub);
        assert_eq!(parse_line("neg"), Instruction::Neg);
        assert_eq!(parse_line("eq"), Instruction::Eq);
        assert_eq!(parse_line("gt"), Instruction::Gt);
        assert_eq!(parse_line("lt"), Instruction::Lt);
        assert_eq!(parse_line("and"), Instruction::And);
        assert_eq!(parse_line("or"), Instruction::Or);
        assert_eq!(parse_line("not"), Instruction::Not);
    }
}
