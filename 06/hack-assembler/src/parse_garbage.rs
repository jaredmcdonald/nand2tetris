// eehhh
let chars = line.chars().peekable();
let mut is_a_instruction = false;
let mut is_c_instruction = false;
let mut a_instruction_value = Vec::new();
let mut c_instruction_lhs = Vec::new();
let mut c_instruction_rhs = Vec::new();
let mut c_instruction_dest = Vec::new();
loop {
    let current = chars.next();
    let next = chars.peek();
    match (current, next) {
        (Some(c), Some(n)) => {
            if c == '/' && n == '/' {
                break; // comment, stop parsing
            } else if c == ' ' || c == '\t' {
                continue; // whitespace, ignore
            } else if c == '@' {
                is_a_instruction = true;
            } else if is_a_instruction {
                a_instruction_value.push(current);
            } else {
                // C-instruction, figure this out
            }
        },
        (None, _) => break, // EOL
    }
}

if is_a_instruction {
    return Some(HackInstruction::AInstruction(AInstruction {
        value: a_instruction_value.collect::<String>()
    }));
} else if is_c_instruction {
    return Some(HackInstruction::CInstruction(CInstruction { comp: "A".to_string(), dest: None, jump: None }))
} else {
    return None;
}
