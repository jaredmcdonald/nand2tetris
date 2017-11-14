// this struct is probably overkill since it just has one field
struct AInstruction {
    value: String,
}

struct CInstruction {
    comp: String,
    dest: Option<String>,
    jump: Option<String>,
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
        let split_at_eq = line.split("=").collect::<Vec<&str>>();
        let eq_len = split_at_eq.len();
        let dest = if eq_len == 1 { None } else { Some(split_at_eq[0].to_string()) }; 
        let split_at_semi = split_at_eq[eq_len - 1].split(";").collect::<Vec<&str>>();
        let jump = if split_at_semi.len() == 1 { None } else { Some(split_at_semi[1].to_string()) };
        let comp = split_at_semi[0].to_string();
        
        HackInstruction::CInstruction(CInstruction { comp, dest, jump })
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

    #[test]
    fn test_simple_c_instruction() {
        match parse_line("0") {
            HackInstruction::CInstruction(instruction) => {
                assert_eq!(instruction.comp, "0");
                assert_eq!(instruction.dest, None);
                assert_eq!(instruction.jump, None);
            },
            _ => panic!("should have parsed a C-instruction"),
        }
    }

    #[test]
    fn test_c_instruction_jmp() {
        match parse_line("D-M;JMP") {
            HackInstruction::CInstruction(instruction) => {
                assert_eq!(instruction.comp, "D-M");
                assert_eq!(instruction.dest, None);
                assert_eq!(instruction.jump.unwrap(), "JMP");
            },
            _ => panic!("should have parsed a C-instruction"),
        }
    }

    #[test]
    fn test_complex_c_instruction() {
        match parse_line("D=D-M;JNZ") {
            HackInstruction::CInstruction(instruction) => {
                assert_eq!(instruction.comp, "D-M");
                assert_eq!(instruction.dest.unwrap(), "D");
                assert_eq!(instruction.jump.unwrap(), "JNZ");
            },
            _ => panic!("should have parsed a C-instruction"),
        }
    }

    #[test]
    fn test_c_instruction_without_jump() {
        match parse_line("D=D-M") {
            HackInstruction::CInstruction(instruction) => {
                assert_eq!(instruction.comp, "D-M");
                assert_eq!(instruction.dest.unwrap(), "D");
                assert_eq!(instruction.jump, None);
            },
            _ => panic!("should have parsed a C-instruction"),
        }
    }
}
