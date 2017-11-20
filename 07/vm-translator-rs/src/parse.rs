#[derive(Debug, PartialEq)]
enum Instruction {
    Push(Push),
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
enum MemorySegment {
    Constant,
    Unknown,
}

#[derive(Debug, PartialEq)]
struct Push {
    segment: MemorySegment,
    index: u16,
}

// assumes the line has already been stripped of whitespace and comments
fn parse_line(line: &str) -> Instruction {
    let space_split = line.split(" ").collect::<Vec<&str>>();
    match space_split[0] {
        "push" => Instruction::Push(Push {
            segment: match space_split[1] {
                "constant" => MemorySegment::Constant,
                _ => MemorySegment::Unknown,
            },
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
