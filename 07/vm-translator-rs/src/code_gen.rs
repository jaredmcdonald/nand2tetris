use parse::{Instruction, Push};
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

fn generate_push(push: &Push) -> Vec<String> {
    // for now ignore memory segment, we know it's `constant`
    let mut asm = vec![
        format!("@{}", push.index), // load the constant into A
        "D=A".to_string(),          // save it in D
    ];
    asm.extend(push_from_d());      // push what's in D to stack
    asm
}

fn generate_line(instruction: &Instruction) -> Vec<String> {
    match instruction {
        &Instruction::Push(ref p) => generate_push(&p), // TODO: why `ref`? the compiler told me to, but...
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
