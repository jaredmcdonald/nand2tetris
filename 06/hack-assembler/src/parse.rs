// this struct is probably overkill since it just has one field
struct AInstruction {
    value: String,
}

struct CInstruction {
    comp: String,
    dest: String,
    jump: String,
}

enum HackInstruction {
    AInstruction(AInstruction),
    CInstruction(CInstruction),
    Comment,
}

fn parse_line(line: &str) -> HackInstruction {
    let as_bytes = line.as_bytes();
    if as_bytes[0] == 47 && as_bytes[1] == 47 { // '//', comment
        HackInstruction::Comment
    } else if as_bytes[0] == 64 { // '@', A-instruction
        HackInstruction::AInstruction(AInstruction { value: line[1..].to_string() })
    } else {
        HackInstruction::CInstruction(CInstruction {
            comp: "".to_string(),
            dest: "".to_string(),
            jump: "".to_string(),
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_a_instruction() {
        match parse_line("@123") {
            HackInstruction::AInstruction(instruction) => assert_eq!(instruction.value, "123"),
            _ => panic!("should have parsed an A-instruction"),
        }
    }

    #[test]
    fn test_comment() {
        match parse_line("// this is a comment") {
            HackInstruction::Comment => assert!(true),
            _ => panic!("should have parsed a comment"),
        }
    }
}
