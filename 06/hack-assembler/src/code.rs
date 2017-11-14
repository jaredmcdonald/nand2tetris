use parse::{HackInstruction, AInstruction, CInstruction};

pub fn generate_line(instruction: HackInstruction) -> String {
    match instruction {
        HackInstruction::AInstruction(i) => a_instruction(i),
        HackInstruction::CInstruction(i) => c_instruction(i),
    }
}

// TODO deal with errors
fn a_instruction(instruction: AInstruction) -> String {
    let value: u16 = instruction.value.parse().unwrap();
    format!("0{:015b}", value)
}

fn c_instruction(instruction: CInstruction) -> String {
    let c_bits = comp(instruction.comp).unwrap();
    let d_bits = dest(instruction.dest).unwrap();
    let j_bits = jump(instruction.jump).unwrap();

    let mut result = vec![1, 1, 1]; // C-instruction prefix (3)
    result.extend(c_bits.iter());   // comp bits (7)
    result.extend(d_bits.iter());   // dest bits (3)
    result.extend(j_bits.iter());   // jump bits (3)

    result.iter().map(|b| format!("{}", b)).collect()
}

fn dest(mnemonic: Option<String>) -> Result<[u8; 3], &'static str> {
    let mut result = [0, 0, 0];

    if let Some(m) = mnemonic {
        let split = m.split("").collect::<Vec<&str>>();
        for destination in split {
            match destination {
                "" => continue,
                "A" => result[0] = 1,
                "D" => result[1] = 1,
                "M" => result[2] = 1,
                _ => return Err("invalid dest mnemonic"), // todo handle invalid destinations like "MM". also should this be in the parser?
            }
        }
    };
    Ok(result)
}


fn jump(mnemonic: Option<String>) -> Result<[u8; 3], &'static str> {
    if let Some(m) = mnemonic {
        match m.as_ref() {
            "JGT" => Ok([0, 0, 1]),
            "JEQ" => Ok([0, 1, 0]),
            "JGE" => Ok([0, 1, 1]),
            "JLT" => Ok([1, 0, 0]),
            "JNE" => Ok([1, 0, 1]),
            "JLE" => Ok([1, 1, 0]),
            "JMP" => Ok([1, 1, 1]),
            _     => Err("invalid jump mnemonic"),
        }
    } else { Ok([0, 0, 0]) }
}

fn comp(mnemonic: String) -> Result<[u8; 7], &'static str> {
    match mnemonic.as_ref() {
        // this is absolute garbage, need to find some sort of pattern to set these bits
        // need to sort of reverse-engineer the ALU? and also figure out an efficient way to parse
        // these... not sure if it's worthwhile
        // could at least collapse the c-bit (the one that selects A vs M), but seems i'd need an
        // external regex crate for that? blargh
        "0"   => Ok([0, 1, 0, 1, 0, 1, 0]),
        "1"   => Ok([0, 1, 1, 1, 1, 1, 1]),
        "-1"  => Ok([0, 1, 1, 1, 0, 1, 0]),
        "D"   => Ok([0, 0, 0, 1, 1, 0, 0]),
        "A"   => Ok([0, 1, 1, 0, 0, 0, 0]),
        "M"   => Ok([1, 1, 1, 0, 0, 0, 0]),
        "!D"  => Ok([0, 0, 0, 1, 1, 0, 1]),
        "!A"  => Ok([0, 1, 1, 0, 0, 0, 1]),
        "!M"  => Ok([1, 1, 1, 0, 0, 0, 1]),
        "-D"  => Ok([0, 0, 0, 1, 1, 1, 1]),
        "-A"  => Ok([0, 1, 1, 0, 0, 1, 1]),
        "-M"  => Ok([1, 1, 1, 0, 0, 1, 1]),
        "D+1" => Ok([0, 0, 1, 1, 1, 1, 1]),
        "A+1" => Ok([0, 1, 1, 0, 1, 1, 1]),
        "M+1" => Ok([1, 1, 1, 0, 1, 1, 1]),
        "D-1" => Ok([0, 0, 0, 1, 1, 1, 0]),
        "A-1" => Ok([0, 1, 1, 0, 0, 1, 0]),
        "M-1" => Ok([1, 1, 1, 0, 0, 1, 0]),
        "D+A" => Ok([0, 0, 0, 0, 0, 1, 0]),
        "D+M" => Ok([1, 0, 0, 0, 0, 1, 0]),
        "D-A" => Ok([0, 0, 1, 0, 0, 1, 1]),
        "D-M" => Ok([1, 0, 1, 0, 0, 1, 1]),
        "A-D" => Ok([0, 0, 0, 0, 1, 1, 1]),
        "M-D" => Ok([1, 0, 0, 0, 1, 1, 1]),
        "D&A" => Ok([0, 0, 0, 0, 0, 0, 0]),
        "D&M" => Ok([1, 0, 0, 0, 0, 0, 0]),
        "D|A" => Ok([0, 0, 1, 0, 1, 0, 1]),
        "D|M" => Ok([1, 0, 1, 0, 1, 0, 1]),
        _     => Err("invalid comp mnemonic"),
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_dest() {
        assert_eq!(dest(None).unwrap(), [0, 0, 0]);
        assert_eq!(dest(Some("AMD".to_string())).unwrap(), [1, 1, 1]);
        assert_eq!(dest(Some("A".to_string())).unwrap(), [1, 0, 0]);
        assert_eq!(dest(Some("D".to_string())).unwrap(), [0, 1, 0]);
        assert_eq!(dest(Some("M".to_string())).unwrap(), [0, 0, 1]);
        assert_eq!(dest(Some("AD".to_string())).unwrap(), [1, 1, 0]);
        assert_eq!(dest(Some("DM".to_string())).unwrap(), [0, 1, 1]);
        assert_eq!(dest(Some("AM".to_string())).unwrap(), [1, 0, 1]);
        match dest(Some("B".to_string())) {
            Err(_) => assert!(true),
            _ => panic!("unknown dest should be an error"),
        };
    }

    #[test]
    fn test_jump() {
        assert_eq!(jump(None).unwrap(), [0, 0, 0]);
        assert_eq!(jump(Some("JGT".to_string())).unwrap(), [0, 0, 1]);
        assert_eq!(jump(Some("JEQ".to_string())).unwrap(), [0, 1, 0]);
        assert_eq!(jump(Some("JGE".to_string())).unwrap(), [0, 1, 1]);
        assert_eq!(jump(Some("JLT".to_string())).unwrap(), [1, 0, 0]);
        assert_eq!(jump(Some("JNE".to_string())).unwrap(), [1, 0, 1]);
        assert_eq!(jump(Some("JLE".to_string())).unwrap(), [1, 1, 0]);
        assert_eq!(jump(Some("JMP".to_string())).unwrap(), [1, 1, 1]);
        match jump(Some("IDK".to_string())) {
            Err(_) => assert!(true),
            _ => panic!("unknown jump directive should be an error"),
        }
    }

    #[test]
    fn test_comp() {
        assert_eq!(comp("0".to_string()).unwrap(), [0, 1, 0, 1, 0, 1, 0]);
        assert_eq!(comp("1".to_string()).unwrap(), [0, 1, 1, 1, 1, 1, 1]);
        assert_eq!(comp("-1".to_string()).unwrap(), [0, 1, 1, 1, 0, 1, 0]);
        assert_eq!(comp("D".to_string()).unwrap(), [0, 0, 0, 1, 1, 0, 0]);
        assert_eq!(comp("A".to_string()).unwrap(), [0, 1, 1, 0, 0, 0, 0]);
        assert_eq!(comp("M".to_string()).unwrap(), [1, 1, 1, 0, 0, 0, 0]);
        assert_eq!(comp("!D".to_string()).unwrap(), [0, 0, 0, 1, 1, 0, 1]);
        assert_eq!(comp("!A".to_string()).unwrap(), [0, 1, 1, 0, 0, 0, 1]);
        assert_eq!(comp("!M".to_string()).unwrap(), [1, 1, 1, 0, 0, 0, 1]);
        assert_eq!(comp("-D".to_string()).unwrap(), [0, 0, 0, 1, 1, 1, 1]);
        assert_eq!(comp("-A".to_string()).unwrap(), [0, 1, 1, 0, 0, 1, 1]);
        assert_eq!(comp("-M".to_string()).unwrap(), [1, 1, 1, 0, 0, 1, 1]);
        assert_eq!(comp("D+1".to_string()).unwrap(), [0, 0, 1, 1, 1, 1, 1]);
        assert_eq!(comp("A+1".to_string()).unwrap(), [0, 1, 1, 0, 1, 1, 1]);
        assert_eq!(comp("M+1".to_string()).unwrap(), [1, 1, 1, 0, 1, 1, 1]);
        assert_eq!(comp("D-1".to_string()).unwrap(), [0, 0, 0, 1, 1, 1, 0]);
        assert_eq!(comp("A-1".to_string()).unwrap(), [0, 1, 1, 0, 0, 1, 0]);
        assert_eq!(comp("M-1".to_string()).unwrap(), [1, 1, 1, 0, 0, 1, 0]);
        assert_eq!(comp("D+A".to_string()).unwrap(), [0, 0, 0, 0, 0, 1, 0]);
        assert_eq!(comp("D+M".to_string()).unwrap(), [1, 0, 0, 0, 0, 1, 0]);
        assert_eq!(comp("D-A".to_string()).unwrap(), [0, 0, 1, 0, 0, 1, 1]);
        assert_eq!(comp("D-M".to_string()).unwrap(), [1, 0, 1, 0, 0, 1, 1]);
        assert_eq!(comp("A-D".to_string()).unwrap(), [0, 0, 0, 0, 1, 1, 1]);
        assert_eq!(comp("M-D".to_string()).unwrap(), [1, 0, 0, 0, 1, 1, 1]);
        assert_eq!(comp("D&A".to_string()).unwrap(), [0, 0, 0, 0, 0, 0, 0]);
        assert_eq!(comp("D&M".to_string()).unwrap(), [1, 0, 0, 0, 0, 0, 0]);
        assert_eq!(comp("D|A".to_string()).unwrap(), [0, 0, 1, 0, 1, 0, 1]);
        assert_eq!(comp("D|M".to_string()).unwrap(), [1, 0, 1, 0, 1, 0, 1]);
        match comp("A++".to_string()) {
            Err(_) => assert!(true),
            _ => panic!("should have returned an error for an invalid mnemonic"),
        };
    }

    #[test]
    fn test_a_instruction() {
        assert_eq!(a_instruction(AInstruction { value: "3".to_string() }), "0000000000000011");
    }

    #[test]
    fn test_c_instruction() {
        let instruction = CInstruction {
            comp: "D+A".to_string(),
            dest: Some("D".to_string()),
            jump: None,
        };
        assert_eq!(c_instruction(instruction), "1110000010010000");
    }
}
