use parse::{Instruction, MemoryLocation, Comparison, Binary, Unary, MemorySegment};
use rand::random;

// convenience enum, because the binary/unary generation code looks a lot alike (but we still should
// be able to distinguish between them - hence not collapsing them at the parse::Instruction level)
#[derive(Debug, PartialEq)]
enum BinaryOrUnary {
    Binary(Binary),
    Unary(Unary),
}

// pop the stack to D register
fn pop_to_d() -> Vec<String> {
    vec![
        "@SP".to_string(),  // load stack pointer
        "M=M-1".to_string(), // decrement it
        "A=M".to_string(),  // dereference
        "D=M".to_string(),  // put it in D
    ]
}

// push to the stack from the D register
fn push_from_d() -> Vec<String> {
    vec![
        "@SP".to_string(),   // load stack pointer
        "A=M".to_string(),   // derefrence
        "M=D".to_string(),   // set it to what's in the D register
        "@SP".to_string(),   // load stack pointer
        "M=M+1".to_string(), // increment
    ]
}

// generate assembly for a comparison operation (eq, gt, lt)
fn generate_comparison_operation(comparison: &Comparison) -> Vec<String> {
    let number_for_label = random::<u64>(); // lol, is this a good idea? maybe a global counter instead?

    // Lt/Gt use the opposite jump instruction because their arguments come off the stack in the
    // "reverse" order; i.e., 6 < 7 will evaluate 7 - 6 and then use the result's relation to zero
    let jump_directive = match comparison {
        &Comparison::Gt => "JLT",
        &Comparison::Lt => "JGT",
        &Comparison::Eq => "JEQ",
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

// generate assembly for a binary or unary operation (bitwise ALU ops)
fn generate_binary_or_unary(instruction: &BinaryOrUnary) -> Vec<String> {
    // there must be a more concise way to do this
    let is_binary = match instruction {
        &BinaryOrUnary::Binary(_) => true,
        &BinaryOrUnary::Unary(_) => false,
    };

    let operator = match instruction {
        &BinaryOrUnary::Binary(Binary::Add) => "+",
        &BinaryOrUnary::Binary(Binary::Sub) => "-",
        &BinaryOrUnary::Binary(Binary::And) => "&",
        &BinaryOrUnary::Binary(Binary::Or) =>  "|",
        &BinaryOrUnary::Unary(Unary::Not) => "!",
        &BinaryOrUnary::Unary(Unary::Neg) => "-",
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
    if instruction == &BinaryOrUnary::Binary(Binary::Sub) {
        // for subtraction, operands are "backwards", so we need to negate the result
        asm.extend(generate_binary_or_unary(&BinaryOrUnary::Unary(Unary::Neg)));
    }
    asm
}

// given a memory segment and index (and filename, for static), generate assembly to put the
// address in the A register
fn get_address_in_a(location: &MemoryLocation, filename: &str) -> Vec<String> {
    // temp and pointer are fixed (RAM 3 and 5, respectively),
    // so just load them into A directly
    if location.segment == MemorySegment::Pointer {
        return vec![format!("@{}", 3 + location.index)];
    } else if location.segment == MemorySegment::Temp {
        return vec![format!("@{}", 5 + location.index)];
    } else if location.segment == MemorySegment::Static {
        // statics are just symbols with the form Filename.index, e.g., MyStupidFile.12
        return vec![format!("@{}.{}", filename, location.index)];
    }

    let asm_base_symbol = match location.segment {
        MemorySegment::Local => "LCL",
        MemorySegment::Argument => "ARG",
        MemorySegment::This => "THIS",
        MemorySegment::That => "THAT",
        // this will never happen (ha) since we've already accounted for the other memory segments
        // above, but the compiler doesn't know about that
        _ => panic!("unimplemented memory segment: {:?}", location.segment),
    };
    vec![
        format!("@{}", location.index), // put the offset (index) into A
        "D=A".to_string(),     // move it to D
        format!("@{}", asm_base_symbol), // load the symbol into A
        "A=M+D".to_string(),   // dereference and add the offset (in D)
    ]
}

// generate assembly for a push instruction
fn generate_push(push: &MemoryLocation, filename: &str) -> Vec<String> {
    let mut asm = if push.segment == MemorySegment::Constant {
        vec![
            format!("@{}", push.index), // load the constant into A
            "D=A".to_string(),          // save it in D
        ]
    } else {
        let mut got_address = get_address_in_a(&push, filename); // get the address of the memory segment in A
        got_address.push("D=M".to_string()); // move it to the D register
        got_address
    };
    asm.extend(push_from_d()); // push what's in D to the stack
    asm
}

// generate assembly for a pop instruction
fn generate_pop(pop: &MemoryLocation, filename: &str) -> Vec<String> {
    let mut asm = get_address_in_a(pop, filename); // load the target address to pop to into A
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

// delegates work for assembly-writing
fn generate_line(instruction: &Instruction, filename: &str) -> Vec<String> {
    match instruction {
        // TODO: why `ref` below? the compiler told me to, but...
        &Instruction::Push(ref push) => generate_push(&push, filename),
        &Instruction::Pop(ref pop) => generate_pop(&pop, filename),
        &Instruction::Binary(ref b) => generate_binary_or_unary(&BinaryOrUnary::Binary(*b)),
        &Instruction::Unary(ref u) => generate_binary_or_unary(&BinaryOrUnary::Unary(*u)),
        &Instruction::Comparison(ref c) => generate_comparison_operation(c),
    }
}

// outer function that takes parsed Instructions and returns generated assembly
pub fn generate(instructions: &[Instruction], filename: &str) -> Vec<String> {
    let mut result = Vec::new();
    for instruction_set in instructions.iter().map(|l| generate_line(l, filename)) {
        result.extend(instruction_set);
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    // this is pretty useless, the integration tests supplied w/ the course are better
    #[test]
    fn test_generate_push() {
        let push = MemoryLocation { segment: MemorySegment::Constant, index: 99 };
        assert_eq!(
            generate_push(&push, "foobar"),
            vec!["@99", "D=A", "@SP", "A=M", "M=D", "@SP", "M=M+1"]
        );
    }
}
