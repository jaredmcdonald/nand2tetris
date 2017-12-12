use parse::{Class, ClassBodyItem, Subroutine};
use symbols::{SymbolTable, SymbolError};

#[derive(Debug, PartialEq)]
pub enum CodeGenError {
    SymbolError(SymbolError),
}

impl From<SymbolError> for CodeGenError {
    fn from(error: SymbolError) -> Self {
        CodeGenError::SymbolError(error)
    }
}

#[derive(Debug, PartialEq)]
pub enum MemorySegment {
    Const,
    Arg,
    Local,
    Static,
    This,
    That,
    Pointer,
    Temp,
}

#[derive(Debug, PartialEq)]
pub enum Arithmetic {
    Add,
    Sub,
    Neg,
    Eq,
    Gt,
    Lt,
    And,
    Or,
    Not,
}

type CodeGenResult = Result<Vec<VmInstruction>, CodeGenError>;

#[derive(Debug, PartialEq)]
pub enum VmInstruction {
    Push(MemorySegment, usize),
    Pop(MemorySegment, usize),
}

fn generate_subroutine(subroutine: &Subroutine, class_symbol_table: &SymbolTable) -> CodeGenResult {
    Ok(vec![])
}

pub fn generate(class: &Class) -> CodeGenResult {
    let mut class_symbol_table = SymbolTable::new();
    let (class_vars, subroutines): (Vec<_>, Vec<_>) = class.body.iter().partition(|i| {
        if let ClassBodyItem::ClassVar(_) = **i { true } else { false }
    });
    // TODO can i just partition into iterators instead of the intermediate vecs?
    let mut class_vars_iter = class_vars.iter();
    while let Some(&&ClassBodyItem::ClassVar(ref var)) = class_vars_iter.next() {
        class_symbol_table.insert(&var)?;
    }

    let mut generated = vec![];
    let mut subroutines_iter = subroutines.iter();
    while let Some(&&ClassBodyItem::Subroutine(ref subroutine)) = subroutines_iter.next() {
        generated.extend(generate_subroutine(subroutine, &class_symbol_table)?)
    }
    Ok(generated)
}
