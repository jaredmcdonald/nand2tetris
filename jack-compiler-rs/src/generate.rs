use ast::*;
use symbols::*;

#[derive(Debug, PartialEq)]
pub enum CodeGenError {
    SymbolError(SymbolError),
    MalformedExpression
}

impl From<SymbolError> for CodeGenError {
    fn from(error: SymbolError) -> Self {
        CodeGenError::SymbolError(error)
    }
}

#[derive(Debug, PartialEq)]
pub enum MemorySegment {
    Constant,
    Argument,
    Local,
    Static,
    This,
    That,
    Pointer,
    Temp,
}

impl From<VarType> for MemorySegment {
    fn from(value: VarType) -> Self {
        match value {
            VarType::Argument => MemorySegment::Argument,
            VarType::Local => MemorySegment::Local,
            VarType::Static => MemorySegment::Static,
            VarType::Field => MemorySegment::This, // wat
        }
    }
}

impl From<BinaryOp> for VmInstruction {
    fn from(value: BinaryOp) -> Self {
        use ast::BinaryOp::*;
        match value {
            Plus => VmInstruction::Add,
            Minus => VmInstruction::Sub,
            And => VmInstruction::And,
            Or => VmInstruction::Or,
            Lt => VmInstruction::Lt,
            Gt => VmInstruction::Gt,
            Eq => VmInstruction::Eq,
            Mult => VmInstruction::Call("Math.multiply".to_owned(), 2),
            Div => VmInstruction::Call("Math.divide".to_owned(), 2),
        }
    }
}

type CodeGenResult = Result<Vec<VmInstruction>, CodeGenError>;

#[derive(Debug, PartialEq)]
pub enum VmInstruction {
    Push(MemorySegment, usize), // segment, index
    Pop(MemorySegment, usize),  // segment, index
    Goto(String),               // label
    IfGoto(String),             // label
    Label(String),              // label (duh)
    Function(String, usize),    // name, local_count
    Call(String, usize),        // name, num_args
    Add,
    Sub,
    And,
    Or,
    Lt,
    Gt,
    Eq,
}

fn generate_term(
    term: &Term,
    symbol_table: &LayeredSymbolTable
) -> CodeGenResult {
    match *term {
        Term::IntegerConstant(i) => Ok(vec![VmInstruction::Push(MemorySegment::Constant, i as usize)]),
        Term::VarName(ref name) => {
            let (target, index) = symbol_table.get(name)?;
            Ok(vec![VmInstruction::Push(MemorySegment::from(target), index)])
        },
        Term::Parenthetical(ref e) => generate_expression(e, symbol_table),
        _ => Ok(vec![])
    }

}

// infix:   ((a + b) + c) + d
// postfix: a b + c + d +
fn generate_expression(
    expression: &Expression,
    symbol_table: &LayeredSymbolTable
) -> CodeGenResult {
    let mut expression_iter = expression.clone().into_iter(); // clone 👎
    let mut result = vec![];
    if let Some(ExpressionItem::Term(term1)) = expression_iter.next() {
        result.extend(generate_term(&term1, symbol_table)?);
    } else {
        return Err(CodeGenError::MalformedExpression);
    }
    while let Some(ExpressionItem::Operation(op)) = expression_iter.next() {
        // term1 op term2 -> term1 term2 op
        if let Some(ExpressionItem::Term(term2)) = expression_iter.next() {
            result.extend(generate_term(&term2, symbol_table)?);
            result.push(VmInstruction::from(op));
        } else {
            return Err(CodeGenError::MalformedExpression);
        }
    }
    Ok(result)
}

fn generate_let_statement(
    statement: &LetStatement,
    symbol_table: &LayeredSymbolTable
) -> CodeGenResult {
    let mut result = generate_expression(&statement.expression, symbol_table)?;
    let (target, index) = symbol_table.get(&statement.name)?;
    result.push(VmInstruction::Pop(MemorySegment::from(target), index));
    Ok(result)
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
        generated_statements.extend(generate_statement(&statement, symbol_table)?);
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

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_generate_term() {
        let st1 = SymbolTable::new();
        let mut st2 = SymbolTable::new();
        st2.insert(&Var {
            names: vec!["blargh".to_owned()],
            data_type: Type::Int,
            var_type: VarType::Argument,
        }).unwrap();
        let symbol_table = LayeredSymbolTable::new(&st1, &st2);

        assert_eq!(
            generate_term(&Term::VarName("blargh".to_owned()), &symbol_table),
            Ok(vec![VmInstruction::Push(MemorySegment::Argument, 0)])
        );

        assert!(generate_term(&Term::VarName("argh".to_owned()), &symbol_table).is_err());
    }

    #[test]
    fn test_generate_expression() {
        let st1 = SymbolTable::new();
        let st2 = SymbolTable::new();
        let empty_symbol_table = LayeredSymbolTable::new(&st1, &st2);

        let expr = Expression(vec![
            ExpressionItem::Term(Term::IntegerConstant(2)),
            ExpressionItem::Operation(BinaryOp::Plus),
            ExpressionItem::Term(Term::IntegerConstant(3)),
        ]);
        let result = generate_expression(&expr, &empty_symbol_table);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), vec![
            VmInstruction::Push(MemorySegment::Constant, 2),
            VmInstruction::Push(MemorySegment::Constant, 3),
            VmInstruction::Add,
        ]);

        let longer_expr = Expression(vec![
            ExpressionItem::Term(Term::IntegerConstant(2)),
            ExpressionItem::Operation(BinaryOp::Plus),
            ExpressionItem::Term(Term::IntegerConstant(3)),
            ExpressionItem::Operation(BinaryOp::Plus),
            ExpressionItem::Term(Term::IntegerConstant(4)),
        ]);
        let longer_expr_result = generate_expression(&longer_expr, &empty_symbol_table);
        assert!(longer_expr_result.is_ok());
        assert_eq!(longer_expr_result.unwrap(), vec![
            VmInstruction::Push(MemorySegment::Constant, 2),
            VmInstruction::Push(MemorySegment::Constant, 3),
            VmInstruction::Add,
            VmInstruction::Push(MemorySegment::Constant, 4),
            VmInstruction::Add,
        ]);
    }

    #[test]
    fn test_generate_parenthetical_expr() {
        let st1 = SymbolTable::new();
        let st2 = SymbolTable::new();
        let empty_symbol_table = LayeredSymbolTable::new(&st1, &st2);

        let expr = Expression(vec![
            // 2 * (3 + 4)
            ExpressionItem::Term(Term::IntegerConstant(2)),
            ExpressionItem::Operation(BinaryOp::Mult),
            ExpressionItem::Term(Term::Parenthetical(
                Expression(vec![
                    ExpressionItem::Term(Term::IntegerConstant(3)),
                    ExpressionItem::Operation(BinaryOp::Plus),
                    ExpressionItem::Term(Term::IntegerConstant(4)),
                ])
            )),
        ]);

        let result = generate_expression(&expr, &empty_symbol_table);
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), vec![
            VmInstruction::Push(MemorySegment::Constant, 2),
            VmInstruction::Push(MemorySegment::Constant, 3),
            VmInstruction::Push(MemorySegment::Constant, 4),
            VmInstruction::Add,
            VmInstruction::Call("Math.multiply".to_owned(), 2),
        ]);
    }
}
