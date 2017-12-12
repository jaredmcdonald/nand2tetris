use parse::{Class, ClassBodyItem, Subroutine, Statement, LetStatement};
use symbols::{SymbolTable, LayeredSymbolTable, SymbolError};

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

fn generate_let_statement(
    statement: &LetStatement,
    symbol_table: &LayeredSymbolTable
) -> CodeGenResult {
    // if statement.index_expression.is_some() { panic!("unimplemented") }
    Ok(vec![])
}

fn generate_statement(
    statement: &Statement,
    symbol_table: &LayeredSymbolTable
) -> CodeGenResult {
    match statement {
        &Statement::Let(ref s) => generate_let_statement(s, symbol_table),
        _ => Ok(vec![]),
    }
}

fn generate_statements(
    statements: &[Statement],
    symbol_table: &LayeredSymbolTable
) -> CodeGenResult {
    let mut generated_statements = vec![];
    for statement in statements {
        generated_statements.extend(
            generate_statement(&statement, symbol_table)?
        );
    }
    Ok(generated_statements)
}

fn generate_subroutine(subroutine: &Subroutine, class_symbol_table: &SymbolTable) -> CodeGenResult {
    let mut subroutine_symbol_table = SymbolTable::new();
    subroutine_symbol_table.insert_many(&subroutine.params)?;
    subroutine_symbol_table.insert_many(&subroutine.body.var_declarations)?;
    let symbol_table = LayeredSymbolTable::new(class_symbol_table, &subroutine_symbol_table);
    Ok(generate_statements(&subroutine.body.statements, &symbol_table)?)
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
