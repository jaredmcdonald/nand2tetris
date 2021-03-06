use parse::{Instruction, MemoryLocation, Comparison, Binary, Unary, MemorySegment, Function, FunctionCall};
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
    vec![ // how to get rid of all the `to_string()`? or should I?
        "@SP".to_string(),   // load stack pointer
        "M=M-1".to_string(), // decrement it
        "A=M".to_string(),   // dereference
        "D=M".to_string(),   // put it in D
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
    let compare = vec![
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
    ];
    asm.extend(compare);
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

    // how to have a constant map instead of needing to evaluate this every time?
    let operator = match instruction {
        &BinaryOrUnary::Binary(Binary::Add) => "+",
        &BinaryOrUnary::Binary(Binary::Sub) => "-",
        &BinaryOrUnary::Binary(Binary::And) => "&",
        &BinaryOrUnary::Binary(Binary::Or) =>  "|",
        &BinaryOrUnary::Unary(Unary::Not) => "!",
        &BinaryOrUnary::Unary(Unary::Neg) => "-",
    };
    let operand = if is_binary { "D" } else { "" };
    let mut asm = if is_binary { // if a binary operation,
        pop_to_d()               // pop first argument off the stack into D
    } else {
        Vec::new()
    };
    let pop_and_op = vec![
        "@SP".to_string(),                     // load the stack pointer
        "M=M-1".to_string(),                   // decrement (pop)
        "A=M".to_string(),                     // dereference
        format!("D={}{}M", operand, operator), // perform operation
    ];
    asm.extend(pop_and_op);
    asm.extend(push_from_d()); // push the result (in D) to the stack
    if instruction == &BinaryOrUnary::Binary(Binary::Sub) {
        // hack: for subtraction, operands are "backwards", so we need to negate the result
        asm.extend(generate_binary_or_unary(&BinaryOrUnary::Unary(Unary::Neg)));
    }
    asm
}

const POINTER_INDEX: u16 = 3;
const TEMP_INDEX: u16 = 5;

// given a memory segment and index (and filename, for static), generate assembly to put the
// address in the A register
fn get_address_in_a(location: &MemoryLocation, filename: &str) -> Vec<String> {
    let asm_base_symbol = match location.segment {
        // pointer, temp, static return early (though kind of janky to do it from inside the `let`)
        // temp and pointer are fixed, so just load them into A directly
        MemorySegment::Pointer => {
            return vec![format!("@{}", POINTER_INDEX + location.index)];
        },
        MemorySegment::Temp => {
            return vec![format!("@{}", TEMP_INDEX + location.index)];
        },
        MemorySegment::Static => {
            // statics are just symbols with the form Filename.index, e.g., MyStupidFile.12
            return vec![format!("@{}.{}", filename, location.index)];
        },
        MemorySegment::Local => "LCL",
        MemorySegment::Argument => "ARG",
        MemorySegment::This => "THIS",
        MemorySegment::That => "THAT",
        // early return for constants, just load it directly into A
        MemorySegment::Constant => return vec![format!("@{}", location.index)],
    };
    vec![
        format!("@{}", location.index),  // put the offset (index) into A
        "D=A".to_string(),               // move it to D
        format!("@{}", asm_base_symbol), // load the symbol into A
        "A=M+D".to_string(),             // dereference and add the offset (in D)
    ]
}

// generate assembly for a push instruction
fn generate_push(push: &MemoryLocation, filename: &str) -> Vec<String> {
    let mut asm = get_address_in_a(&push, filename); // get address of memory segment in A (or the constant itself)
    if push.segment == MemorySegment::Constant {
        asm.push("D=A".to_string()); // move constant directly into D
    } else {
        asm.push("D=M".to_string()); // otherwise move what's at address in A to the D register
    }
    asm.extend(push_from_d());       // finally, push what's in D to the stack
    asm
}

// generate assembly for a pop instruction
fn generate_pop(pop: &MemoryLocation, filename: &str) -> Vec<String> {
    let temp_ram = "@R13";
    let mut asm = get_address_in_a(pop, filename); // load the target address to pop to into A
    let store_in_temp = vec![
        "D=A".to_string(),    // move address into D
        temp_ram.to_string(), // load up R13 address into A
        "M=D".to_string(),    // store address in R13 because our computer sucks and doesn't have enough registers
    ];
    asm.extend(store_in_temp);
    asm.extend(pop_to_d());   // pop the stack into the D register
    let save_to_memory = vec![
        temp_ram.to_string(), // get R13 in A register
        "A=M".to_string(),    // dereference (this is where we put the target address, above)
        "M=D".to_string(),    // save to memory
    ];
    asm.extend(save_to_memory);
    asm
}

// generate an unconditional goto
fn generate_goto(label: &str) -> Vec<String> {
    vec![
        format!("@{}", label), // load label into A register
        "0;JMP".to_string(),   // unconditional jump
    ]
}

// generate an if-goto (goto supplied label if whatever's at the top of the stack is nonzero)
fn generate_if_goto(label: &str) -> Vec<String> {
    let mut asm = pop_to_d();  // pop the stack into the D register
    let conditional_jump = vec![
        format!("@{}", label), // load label into A register
        "D;JNE".to_string(),   // jump if whatever's in D is nonzero
    ];
    asm.extend(conditional_jump);
    asm
}

// generate a label (ignoring the requirement of FunctionName$label in case it's in a function,
// don't really want to rewrite the parser. how do you know you're "in a function" in VM code anyway?)
fn generate_label(label: &str) -> Vec<String> {
    vec![format!("({})", label)]
}

// generates assembly to restore caller memory, given which symbol and its offset from FRAME
fn restore_symbol_for_caller(symbol: &str, frame_symbol: &str, offset: u8) -> Vec<String> {
    vec![
        frame_symbol.to_string(),
        "D=M".to_string(),   // put FRAME into D
        format!("@{}", offset),
        "D=D-A".to_string(), // calculate FRAME-offset in D
        "A=D".to_string(),   // we want what's at the address in D, so dereference by shuffling into A
        "D=M".to_string(),   // and then put what's there back in M
        format!("@{}", symbol),
        "M=D".to_string(),   // then put it at the memory location specified by `symbol`
    ]
}

fn generate_return() -> Vec<String> {
    let frame = "@R13";
    let ret = "@R14";
    let mut asm = vec![
        "@LCL".to_string(),  // load LCL in A
        "D=M".to_string(),   // *LCL into D
        frame.to_string(),   // load up temp variable for FRAME (above, R13) in A
        "M=D".to_string(),   // put what we had in D, *LCL, into FRAME (R13)

        "@5".to_string(),    // load constant 5
        "D=D-A".to_string(), // subtract A register (5) from the previous *LCL value
        "A=D".to_string(),   // dereference
        "D=M".to_string(),   // put back in D (it's the return address pushed by the call)
        ret.to_string(),     // load temp variable for RET (above, R14) in A
        "M=D".to_string(),   // store what was in D, *(FRAME - 5), in RET (R14)
    ];
    asm.extend(pop_to_d()); // |
    let next_step = vec![   // | reposition the return value:
        "@ARG".to_string(), // |   pop to D register and store at *ARG, aka, *ARG = pop()
        "A=M".to_string(),  // |
        "M=D".to_string(),  // |

        "@ARG".to_string(),  // |
        "D=M+1".to_string(), // |
        "@SP".to_string(),   // | restore stack pointer of caller: SP = ARG+1
        "M=D".to_string(),   // |
    ];
    asm.extend(next_step);

    // restore THAT, THIS, ARG, LCL to *(FRAME - respective offset)
    for &(symbol, offset) in [("THAT", 1), ("THIS", 2), ("ARG", 3), ("LCL", 4)].iter() {
        asm.extend(restore_symbol_for_caller(symbol, frame, offset));
    }
    let jump_to_return = vec![
        ret.to_string(),     // load up return address
        "A=M".to_string(),   // dereference it
        "0;JMP".to_string(), // jump there
    ];
    asm.extend(jump_to_return);
    asm
}

// generate assembly for a function declaration
fn generate_function(function: &Function) -> Vec<String> {
    let mut asm = generate_label(&function.name);  // label it
    for _ in 0..function.local_count {
        asm.extend(generate_push(&MemoryLocation { // push 0, `local_count` times
            segment: MemorySegment::Constant,
            index: 0,
        }, ""))
    }
    asm
}

// generate assembly for a call instruction
fn generate_call(call: &FunctionCall) -> Vec<String> {
    let return_addr = format!("call-{}-{:x}", call.name, random::<u64>());
    let mut asm = vec![
        format!("@{}", return_addr),
        "D=A".to_string(),     // put the return address label into D...
    ];
    asm.extend(push_from_d()); // ...then push it to the stack

    // push LCL, ARG, THIS n THAT
    for symbol in ["LCL", "ARG", "THIS", "THAT"].iter() {
        let pushed_symbol = vec![
            format!("@{}", symbol),
            "D=M".to_string(),
        ];
        asm.extend(pushed_symbol);
        asm.extend(push_from_d());
    }

    let move_arg_and_reset_lcl = vec![
        // move ARG by 5 + number of args
        "@SP".to_string(),
        "D=M".to_string(),
        format!("@{}", 5 + call.arg_count),
        "D=D-A".to_string(),
        "@ARG".to_string(),
        "M=D".to_string(),

        // set LCL to SP
        "@SP".to_string(),
        "D=M".to_string(),
        "@LCL".to_string(),
        "M=D".to_string(),
    ];
    asm.extend(move_arg_and_reset_lcl);
    asm.extend(generate_goto(&call.name));  // jump to function
    asm.push(format!("({})", return_addr)); // finally, add the return address label
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
        &Instruction::Goto(ref l) => generate_goto(&l),
        &Instruction::IfGoto(ref l) => generate_if_goto(&l),
        &Instruction::Label(ref l) => generate_label(&l),
        &Instruction::Function(ref f) => generate_function(&f),
        &Instruction::Return => generate_return(),
        &Instruction::Call(ref c) => generate_call(&c),
    }
}

// outer function that takes parsed Instructions and returns generated assembly
pub fn generate(instructions: &[Instruction], filename: &str) -> Vec<String> {
    instructions.iter().flat_map(|l| generate_line(l, filename)).collect()
}

// generates assembly for system bootstrap - sets up stack pointer and calls Sys.init
pub fn bootstrap() -> Vec<String> {
    let mut asm = vec![
        "@256".to_string(),
        "D=A".to_string(),
        "@SP".to_string(),
        "M=D".to_string(), // initialize stack pointer to RAM[256]
    ];
    asm.extend(generate_call(&FunctionCall { // and call Sys.init()
        name: "Sys.init".to_string(),
        arg_count: 0,
    }));
    asm
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

    #[test]
    fn test_generate_label() {
        assert_eq!(generate_label("blargh"), vec!["(blargh)"]);
    }
}
