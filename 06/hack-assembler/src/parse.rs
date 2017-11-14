// this struct is probably overkill since it just has one field
#[derive(Debug)]
pub struct AInstruction {
    value: String,
}

#[derive(Debug)]
pub struct CInstruction {
    comp: String,
    dest: Option<String>,
    jump: Option<String>,
}

#[derive(Debug)]
pub enum HackInstruction {
    AInstruction(AInstruction),
    CInstruction(CInstruction),
}

fn parse_line(line: &str) -> Option<HackInstruction> {
    let without_comment = line.split("//").collect::<Vec<&str>>(); // remove everything after '//'
    let trimmed = without_comment[0].trim();
    let as_bytes = trimmed.as_bytes();
    if as_bytes.len() == 0 {
        None
    } else if as_bytes[0] == 64 { // '@', A-instruction
        Some(HackInstruction::AInstruction(AInstruction { value: trimmed[1..].to_string() }))
    } else {
        let split_at_eq = trimmed.split("=").collect::<Vec<&str>>();
        let eq_len = split_at_eq.len();
        let dest = if eq_len == 1 { None } else { Some(split_at_eq[0].to_string()) }; 
        let split_at_semi = split_at_eq[eq_len - 1].split(";").collect::<Vec<&str>>();
        let jump = if split_at_semi.len() == 1 { None } else { Some(split_at_semi[1].to_string()) };
        let comp = split_at_semi[0].to_string();
        
        Some(HackInstruction::CInstruction(CInstruction { comp, dest, jump }))
    }
}

pub fn parse_lines(lines: &[String]) -> Vec<HackInstruction> {
    lines.iter().filter_map(|line| parse_line(&line)).collect()
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_a_instruction() {
        match parse_line("@123") {
            Some(HackInstruction::AInstruction(instruction)) => assert_eq!(instruction.value, "123"),
            _ => panic!("should have parsed an A-instruction"),
        }
    }

    #[test]
    fn test_comment() {
        match parse_line("// this is a comment") {
            None => assert!(true),
            _ => panic!("should have returned None for a comment"),
        }
    }

    #[test]
    fn test_instruction_with_trailing_comment() {
        match parse_line("@123 // put 123 in the A register") {
            Some(HackInstruction::AInstruction(instruction)) => assert_eq!(instruction.value, "123"),
            _ => panic!("should have parsed an A-instruction"),
        }
    }

    #[test]
    fn test_simple_c_instruction() {
        match parse_line("0") {
            Some(HackInstruction::CInstruction(instruction)) => {
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
            Some(HackInstruction::CInstruction(instruction)) => {
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
            Some(HackInstruction::CInstruction(instruction)) => {
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
            Some(HackInstruction::CInstruction(instruction)) => {
                assert_eq!(instruction.comp, "D-M");
                assert_eq!(instruction.dest.unwrap(), "D");
                assert_eq!(instruction.jump, None);
            },
            _ => panic!("should have parsed a C-instruction"),
        }
    }

    #[test]
    fn test_parse_empty() {
        match parse_line("") {
            None => assert!(true),
            _ => panic!("should have parsed an empty line"),
        }
    }

    #[test]
    fn test_parse_empty_trim() {
        match parse_line(" ") {
            None => assert!(true),
            _ => panic!("should have parsed an empty line"),
        }
    }

    #[test]
    fn test_parse_lines_filter() {
        let lines = vec![" ".to_string(), "// Hello World".to_string(), "A=A-D;JNZ".to_string(), "@999".to_string()];
        let result = parse_lines(lines.as_slice());
        assert_eq!(result.len(), 2);
    }

}

