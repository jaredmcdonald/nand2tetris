use parse::{Instruction, Push, Pop, MemorySegment};
use rand::random;

// pop the stack to D register
fn pop_to_d() -> Vec<String> {
    vec![
        "@SP".to_string(),  // load stack pointer
        "M=M-1".to_string(), // decrement it
        "A=M".to_string(),  // dereference
        "D=M".to_string(),  // put it in D
    ]
}

fn push_from_d() -> Vec<String> {
    vec![
        "@SP".to_string(),   // load stack pointer
        "A=M".to_string(),   // derefrence
        "M=D".to_string(),   // set it to what's in the D register
        "@SP".to_string(),   // load stack pointer
        "M=M+1".to_string(), // increment
    ]
}

fn generate_comparison_operation(instruction: Instruction) -> Vec<String> {
    let number_for_label = random::<u64>(); // lol, is this a good idea? maybe a global counter instead?

    let jump_directive = match instruction {
        Instruction::Gt => "JLT", // Lt/Gt use the opposite jump instruction because their arguments
        Instruction::Lt => "JGT", // come off the stack in the "reverse" order; i.e., 6 < 7 will evaluate
        Instruction::Eq => "JEQ", // 7 - 6 and then use the result's relation to zero
        _ => panic!("not a comparison operation: {:?}", instruction),
    };

    let true_branch_label = format!("true.{:x}", number_for_label);
    let continue_label = format!("continue.{:x}", number_for_label);

    let mut asm = pop_to_d();
    asm.extend(vec![
        "@SP".to_string(),           // load the stack pointer
        "M=M-1".to_string(),         // decrement (pop)
        "A=M".to_string(),           // dereference
        "D=D-M".to_string(),         // compare
        format!("@{}", true_branch_label),
        format!("D;{}", jump_directive),
        "D=0".to_string(),           // 0 means false
        format!("@{}", continue_label),
        "0;JMP".to_string(),         // hop over the next few statements
        format!("({})", true_branch_label),
        "D=-1".to_string(),          // -1 means true
        format!("({})", continue_label),
    ]);
    asm.extend(push_from_d());
    asm
}

// bad name for this function, todo rename
fn generate_operation(instruction: Instruction) -> Vec<String> {
    let (operator, is_binary) = match instruction {
        Instruction::Add => ("+", true),
        Instruction::Sub => ("-", true),
        Instruction::And => ("&", true),
        Instruction::Or =>  ("|", true),
        Instruction::Not => ("!", false),
        Instruction::Neg => ("-", false),
        _ => panic!("don't know how to deal with this operation: {:?}"),
    };
    let operand = if is_binary { "D" } else { "" };
    let mut asm = if is_binary {     // if a binary operation,
        pop_to_d()                   // pop first argument off the stack into D
    } else {
        Vec::new()
    };
    asm.extend(vec![
        "@SP".to_string(),           // load the stack pointer
        "M=M-1".to_string(),         // decrement (pop)
        "A=M".to_string(),           // dereference
        format!("D={}{}M", operand, operator), // perform operation
    ]);
    asm.extend(push_from_d());       // push the result (in D) to the stack
    if instruction == Instruction::Sub {
        // for subtraction, operands are "backwards", so we need to negate the result
        asm.extend(generate_operation(Instruction::Neg));
    }
    asm
}

fn get_address_in_a(segment: &MemorySegment, index: u16) -> Vec<String> {
    // temp and pointer are fixed (RAM 3 and 5, respectively),
    // so just load them into A directly
    if segment == &MemorySegment::Pointer {
        return vec![format!("@{}", 3 + index)];
    } else if segment == &MemorySegment::Temp {
        return vec![format!("@{}", 5 + index)];
    }

    let asm_base_symbol = match segment {
        &MemorySegment::Local => "LCL",
        &MemorySegment::Argument => "ARG",
        &MemorySegment::This => "THIS",
        &MemorySegment::That => "THAT",
        _ => panic!("unrecognized/unimplemented memory segment"), // this should probably happen at parse time
    };
    vec![
        format!("@{}", index), // put the offset (index) into A
        "D=A".to_string(),     // move it to D
        format!("@{}", asm_base_symbol), // load the symbol into A
        "A=M+D".to_string(),   // dereference and add the offset (in D)
    ]
}

fn generate_push(push: &Push) -> Vec<String> {
    let mut asm = if push.segment == MemorySegment::Constant {
        vec![
            format!("@{}", push.index), // load the constant into A
            "D=A".to_string(),          // save it in D
        ]
    } else {
        let mut got_address = get_address_in_a(&push.segment, push.index); // get the address of the memory segment in A
        got_address.push("D=M".to_string()); // move it to the D register
        got_address
    };
    asm.extend(push_from_d()); // push what's in D to the stack
    asm
}

fn generate_pop(pop: &Pop) -> Vec<String> {
    let mut asm = get_address_in_a(&pop.segment, pop.index); // load the target address to pop to into A
    asm.extend(
        vec![
            "D=A".to_string(),  // move address into D
            "@R13".to_string(), // load up R13 address into A
            "M=D".to_string(),  // store address in R13 because our computer sucks and doesn't have enough registers
        ]
    );
    asm.extend(pop_to_d()); // pop the stack into the D register
    asm.extend(
        vec![
            "@R13".to_string(), // get R13 in A register
            "A=M".to_string(),  // dereference (this is where we put the target address, above)
            "M=D".to_string(),  // save to memory
        ]
    );
    asm
}

fn generate_line(instruction: &Instruction) -> Vec<String> {
    match instruction {
        &Instruction::Push(ref push) => generate_push(&push), // TODO: why `ref`? the compiler told me to, but...
        &Instruction::Pop(ref pop) => generate_pop(&pop),
        &Instruction::Add => generate_operation(Instruction::Add),
        &Instruction::Sub => generate_operation(Instruction::Sub),
        &Instruction::And => generate_operation(Instruction::And),
        &Instruction::Or => generate_operation(Instruction::Or),
        &Instruction::Not => generate_operation(Instruction::Not),
        &Instruction::Neg => generate_operation(Instruction::Neg),
        &Instruction::Gt => generate_comparison_operation(Instruction::Gt),
        &Instruction::Lt => generate_comparison_operation(Instruction::Lt),
        &Instruction::Eq => generate_comparison_operation(Instruction::Eq),
        _ => vec![],
    }
}

pub fn generate(instructions: &[Instruction]) -> Vec<Vec<String>> {
    instructions.iter().map(|l| generate_line(l)).collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_push() {
        let push = Push { segment: MemorySegment::Constant, index: 99 };
        assert_eq!(
            generate_push(&push),
            vec!["@99", "D=A", "@SP", "A=M", "M=D", "@SP", "M=M+1"]
        );
    }
}
