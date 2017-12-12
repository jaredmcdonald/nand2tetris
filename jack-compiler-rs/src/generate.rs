use parse::{Class, ClassBodyItem};
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

#[derive(Debug, PartialEq)]
pub enum VmInstruction {
    Push(MemorySegment, usize),
    Pop(MemorySegment, usize),
}

pub fn generate(class: &Class) -> Result<Vec<VmInstruction>, CodeGenError> {
    let mut class_symbol_table = SymbolTable::new();
    let (class_vars, subroutines): (Vec<_>, Vec<_>) = class.body.iter().partition(|i| {
        if let ClassBodyItem::ClassVar(_) = **i { true } else { false }
    });
    // TODO can i just partition into iterators instead of the intermediate vecs?
    let mut class_vars_iter = class_vars.iter();
    while let Some(&&ClassBodyItem::ClassVar(ref var)) = class_vars_iter.next() {
        class_symbol_table.insert(&var)?;
    }
    Ok(vec![])
}
