// this struct is probably overkill since it just has one field
#[derive(Debug, Clone)]
pub struct AInstruction {
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct CInstruction {
    pub comp: String,
    pub dest: Option<String>,
    pub jump: Option<String>,
}

#[derive(Debug)]
pub struct LabeledLine {
    pub instruction: HackInstruction,
    pub labels: Vec<Label>,
}

#[derive(Debug, Clone)]
pub struct Label {
    pub name: String,
}

#[derive(Debug)]
enum HackLine {
    AInstruction(AInstruction),
    CInstruction(CInstruction),
    Label(Label),
}

#[derive(Debug, Clone)]
pub enum HackInstruction {
    AInstruction(AInstruction),
    CInstruction(CInstruction),
}

fn parse_line(line: &str) -> Option<HackLine> {
    let without_comment = line.split("//").collect::<Vec<&str>>(); // remove everything after '//'
    let trimmed = without_comment[0].trim();
    let as_bytes = trimmed.as_bytes();
    let len = as_bytes.len();
    if len == 0 {
        None
    } else if as_bytes[0] == 64 { // '@', A-instruction
        Some(HackLine::AInstruction(AInstruction { value: trimmed[1..].to_string() }))
    } else if as_bytes[0] == 40 { // '(', label
        Some(HackLine::Label(Label { name: trimmed[1..len - 1].to_string() })) }
    else {
        let split_at_eq = trimmed.split("=").collect::<Vec<&str>>();
        let eq_len = split_at_eq.len();
        let dest = if eq_len == 1 { None } else { Some(split_at_eq[0].to_string()) };
        let split_at_semi = split_at_eq[eq_len - 1].split(";").collect::<Vec<&str>>();
        let jump = if split_at_semi.len() == 1 { None } else { Some(split_at_semi[1].to_string()) };
        let comp = split_at_semi[0].to_string();

        Some(HackLine::CInstruction(CInstruction { comp, dest, jump }))
    }
}

pub fn parse_lines(lines: &[String]) -> Vec<LabeledLine> {
    let mut parsed = lines.iter().filter_map(|line| parse_line(&line));
    let mut with_labels = Vec::new();
    let mut labels_stack = Vec::new();
    loop {
        let current = parsed.next();
        match current {
            // ugh, y so verbose, how to condense?
            // i basically just want to keep the HackLine type internal to the parser,
            // and have the external representation be LabeledLine, with the A/C instruction types
            Some(HackLine::Label(label)) => {
                labels_stack.push(label);
                // let instruction = parsed.next().unwrap();
                // with_labels.push(LabeledLine {
                //     label: Some(label),
                //     instruction: match instruction {
                //         HackLine::AInstruction(i) => HackInstruction::AInstruction(i),
                //         HackLine::CInstruction(i) => HackInstruction::CInstruction(i),
                //         _ => panic!("y u got 2 labels in a row???"), // is this valid? maybe
                //     },
                // });
            },
            Some(line) => {
                with_labels.push(LabeledLine {
                    labels: labels_stack.clone(),
                    instruction: match line {
                        HackLine::AInstruction(i) => HackInstruction::AInstruction(i),
                        HackLine::CInstruction(i) => HackInstruction::CInstruction(i),
                        _ => panic!("this won't happen because we've already tested for label in the surrounding match (famous last words)"),
                    }
                });
                labels_stack = Vec::new(); // reset the label accumulator
            },
            None => break,
        }
    }
    with_labels
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_a_instruction() {
        match parse_line("@123") {
            Some(HackLine::AInstruction(instruction)) => assert_eq!(instruction.value, "123"),
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
    fn test_label() {
        match parse_line("  (BLARGH)// this is a comment") {
            Some(HackLine::Label(label)) => assert_eq!(label.name, "BLARGH"),
            _ => panic!("should have parsed a label"),
        }
    }

    #[test]
    fn test_instruction_with_trailing_comment() {
        match parse_line("@123 // put 123 in the A register") {
            Some(HackLine::AInstruction(instruction)) => assert_eq!(instruction.value, "123"),
            _ => panic!("should have parsed an A-instruction"),
        }
    }

    #[test]
    fn test_simple_c_instruction() {
        match parse_line("0") {
            Some(HackLine::CInstruction(instruction)) => {
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
            Some(HackLine::CInstruction(instruction)) => {
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
            Some(HackLine::CInstruction(instruction)) => {
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
            Some(HackLine::CInstruction(instruction)) => {
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

    #[test]
    fn test_parse_lines_label() {
        let lines = vec!["(BLARGH)".to_string(), "// Hello World".to_string(), "A=A-D;JNZ".to_string()];
        let result = parse_lines(lines.as_slice());
        assert_eq!(result.len(), 1);
        let line = &result[0];
        assert_eq!(line.labels.len(), 1);
        assert_eq!(line.labels[0].name, "BLARGH");
        match line.instruction {
            HackInstruction::CInstruction(_) => assert!(true),
            _ => panic!("should be a C-instruction"),
        }
    }

}
