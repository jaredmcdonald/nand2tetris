use rand::random;
use std::collections::HashMap;
use tokenize::Keyword;
use ast::*;
use symbols::*;

#[derive(Debug, PartialEq)]
pub struct SubroutineEnvironment<'a> {
    symbol_table: LayeredSymbolTable<'a>,
    class_subroutines: &'a HashMap<String, SubroutineType>,
    current_classname: String,
}

#[derive(Debug, PartialEq)]
pub enum CodeGenError {
    SymbolError(SymbolError),
    MalformedExpression,
    UnknownVarName,
    UnknownSubroutineName,
    MethodCallOnPrimitive,
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

impl From<UnaryOp> for VmInstruction {
    fn from(value: UnaryOp) -> Self {
        match value {
            UnaryOp::Not => VmInstruction::Not,
            UnaryOp::Neg => VmInstruction::Neg,
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
    Not,
    Neg,
}

fn generate_term(
    term: &Term,
    environment: &SubroutineEnvironment
) -> CodeGenResult {
    match *term {
        Term::IntegerConstant(i) => Ok(vec![VmInstruction::Push(MemorySegment::Constant, i as usize)]),
        Term::VarName(ref name) => {
            let (target, index, _) = environment.symbol_table.get(name).ok_or(CodeGenError::UnknownVarName)?;
            Ok(vec![VmInstruction::Push(MemorySegment::from(target), index)])
        },
        Term::KeywordConstant(k) => Ok(match k {
            Keyword::True => vec![VmInstruction::Push(MemorySegment::Constant, 1), VmInstruction::Neg],
            _ => vec![VmInstruction::Push(MemorySegment::Constant, 0)] // null, false
        }),
        Term::Unary(op, ref term) => {
            // postfix: -3 -> 3 neg
            let mut result = generate_term(term, environment)?;
            result.push(VmInstruction::from(op));
            Ok(result)
        },
        Term::Parenthetical(ref e) => generate_expression(e, environment),
        _ => Ok(vec![])
    }
}

// infix:   a + b + c + d
// postfix: a b + c + d +
fn generate_expression(
    expression: &Expression,
    environment: &SubroutineEnvironment
) -> CodeGenResult {
    let mut expression_iter = expression.clone().into_iter(); // clone 👎
    let mut result = vec![];
    if let Some(ExpressionItem::Term(term1)) = expression_iter.next() {
        result.extend(generate_term(&term1, environment)?);
    } else {
        return Err(CodeGenError::MalformedExpression);
    }
    while let Some(ExpressionItem::Operation(op)) = expression_iter.next() {
        // postfix: op termN -> termN op
        if let Some(ExpressionItem::Term(term2)) = expression_iter.next() {
            result.extend(generate_term(&term2, environment)?);
            result.push(VmInstruction::from(op));
        } else {
            return Err(CodeGenError::MalformedExpression);
        }
    }
    Ok(result)
}

fn generate_let_statement(
    statement: &LetStatement,
    environment: &SubroutineEnvironment
) -> CodeGenResult {
    let (target, index, _) = environment.symbol_table.get(&statement.name).ok_or(CodeGenError::UnknownVarName)?;
    let rhs_expression = generate_expression(&statement.expression, environment)?;

    if let Some(ref index_expr) = statement.index_expression {
        let mut result = vec![VmInstruction::Push(MemorySegment::from(target), index)];
        result.extend(generate_expression(index_expr, environment)?);
        result.push(VmInstruction::Add);
        // pointer 1 now contains the address of a[b]
        result.push(VmInstruction::Pop(MemorySegment::Pointer, 1)); // TODO is the index always 1? see p.229
        result.extend(rhs_expression);
        result.push(VmInstruction::Pop(MemorySegment::That, 0)); // TODO is the index always 0?
        Ok(result)
    } else {
        let mut result = vec![];
        result.extend(rhs_expression);
        result.push(VmInstruction::Pop(MemorySegment::from(target), index));
        Ok(result)
    }
}

fn generate_if_statement(
    statement: &IfStatement,
    environment: &SubroutineEnvironment
) -> CodeGenResult {
    let mut result = generate_expression(&statement.condition, environment)?; // condition
    let unique = random::<u64>();
    let end_if_label = format!("endif.{:x}", unique);

    result.push(VmInstruction::Not); // invert it because we're deciding whether to skip
    result.push(VmInstruction::IfGoto(end_if_label.to_owned()));
    result.extend(generate_statements(&statement.if_body, environment)?);

    if let Some(ref else_body) = statement.else_body {
        let end_else_label = format!("endelse.{:x}", unique);
        // if we're in the if condition, jump over the else body
        result.push(VmInstruction::Goto(end_else_label.to_owned()));
        // otherwise we will end up here, where we put the else statements
        result.push(VmInstruction::Label(end_if_label.to_owned()));
        result.extend(generate_statements(&else_body, environment)?);
        // here is where we will land if the if condition evaluated to true
        result.push(VmInstruction::Label(end_else_label.to_owned()));
    } else {
        // if there's no else body, just need this label at the end
        result.push(VmInstruction::Label(end_if_label.to_owned()));
    }
    Ok(result)
}

fn generate_while_statement(
    statement: &WhileStatement,
    environment: &SubroutineEnvironment
) -> CodeGenResult {
    let unique = random::<u64>();
    let begin_while_label = format!("beginwhile.{:x}", unique);
    let end_while_label = format!("endwhile.{:x}", unique);
    let mut result = vec![VmInstruction::Label(begin_while_label.to_owned())];
    // evaluate condition
    result.extend(generate_expression(&statement.condition, environment)?);
    // if condition is true, hop over everything
    result.push(VmInstruction::IfGoto(end_while_label.to_owned()));
    // statement body
    result.extend(generate_statements(&statement.body, environment)?);
    // loop back to right above condition
    result.push(VmInstruction::Goto(begin_while_label.to_owned()));
    // end label
    result.push(VmInstruction::Label(end_while_label.to_owned()));
    Ok(result)
}

fn generate_subroutine_call(
    subroutine_call: &SubroutineCall,
    environment: &SubroutineEnvironment
) -> CodeGenResult {
    let mut num_args = subroutine_call.parameters.len();
    let mut result = vec![];
    let mut parent_name = String::new();
    if let Some(ref parent) = subroutine_call.parent_name {
        // this is some other object's function
        if let Some((var_type, index, data_type)) = environment.symbol_table.get(parent) {
            // method call, pass the var as the first arg
            result.push(VmInstruction::Push(MemorySegment::from(var_type), index));
            num_args += 1;
            // the parent name for the function will be the parent classname
            if let Type::Class(ref classname) = data_type {
                parent_name.push_str(classname);
            } else {
                return Err(CodeGenError::MethodCallOnPrimitive);
            }
        } else {
            parent_name.push_str(parent);
        }
    } else {
        // the subroutine resides on this same class (I think?)
        let is_method = *environment.class_subroutines
            .get(&subroutine_call.subroutine_name)
            .ok_or(CodeGenError::UnknownSubroutineName)? == SubroutineType::Method;
        if is_method {
            // TODO somehow `this` needs to be the first argument, how?
            num_args += 1;
        }
        parent_name.push_str(&environment.current_classname);
    }

    for param in &subroutine_call.parameters {
        result.extend(generate_expression(&param, environment)?)
    }

    let name = format!("{}.{}",
        parent_name,
        subroutine_call.subroutine_name
    );

    result.push(VmInstruction::Call(name, num_args));

    Ok(result)
}

fn generate_do_statement(
    subroutine_call: &SubroutineCall,
    environment: &SubroutineEnvironment
) -> CodeGenResult {
    let mut result = generate_subroutine_call(subroutine_call, environment)?;
    result.push(VmInstruction::Pop(MemorySegment::Temp, 0)); // ignore return value
    Ok(result)
}

fn generate_statement(
    statement: &Statement,
    environment: &SubroutineEnvironment
) -> CodeGenResult {
    match *statement {
        Statement::Let(ref s) => generate_let_statement(s, environment),
        Statement::If(ref s) => generate_if_statement(s, environment),
        Statement::While(ref s) => generate_while_statement(s, environment),
        Statement::Do(ref subr_call) => generate_do_statement(subr_call, environment),
        _ => Ok(vec![]),
    }
}

fn generate_statements(
    statements: &[Statement],
    environment: &SubroutineEnvironment
) -> CodeGenResult {
    let mut generated_statements = vec![];
    for statement in statements {
        generated_statements.extend(generate_statement(&statement, environment)?);
    }
    Ok(generated_statements)
}

fn generate_subroutine(
    subroutine: &Subroutine,
    classname: &str,
    class_symbol_table: &SymbolTable,
    class_subroutines: &HashMap<String, SubroutineType>
) -> CodeGenResult {
    let mut subroutine_symbol_table = SymbolTable::new();
    subroutine_symbol_table.insert_many(&subroutine.params)?;
    subroutine_symbol_table.insert_many(&subroutine.body.var_declarations)?;
    let symbol_table = LayeredSymbolTable::new(class_symbol_table, &subroutine_symbol_table);
    let environment = SubroutineEnvironment {
        symbol_table,
        class_subroutines,
        current_classname: classname.to_owned(),
    };
    // todo: insert function declaration, return void logic
    let local_count = subroutine.body.var_declarations.iter().fold(0, |sum, val| sum + val.names.len());
    let function_name = format!("{}.{}", classname, &subroutine.name);
    let mut result = vec![VmInstruction::Function(function_name, local_count)];
    result.extend(generate_statements(&subroutine.body.statements, &environment)?);
    Ok(result)
}

fn get_subroutine_map(subroutines: &[&ClassBodyItem]) -> HashMap<String, SubroutineType> {
    let mut map = HashMap::new();
    let mut subroutines_iter = subroutines.iter();
    while let Some(&&ClassBodyItem::Subroutine(ref subroutine)) = subroutines_iter.next() {
        map.insert(subroutine.name.to_owned(), subroutine.subroutine_type);
    }
    map
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
    let class_subroutines = get_subroutine_map(subroutines.as_slice());
    let mut subroutines_iter = subroutines.iter();
    while let Some(&&ClassBodyItem::Subroutine(ref subroutine)) = subroutines_iter.next() {
        generated.extend(generate_subroutine(subroutine, &class.name, &class_symbol_table, &class_subroutines)?)
    }
    Ok(generated)
}

#[cfg(test)]
mod test {
    use std::panic;
    use super::*;

    // credit to https://medium.com/@ericdreichert/test-setup-and-teardown-in-rust-without-a-framework-ba32d97aa5ab
    fn run_test_with_environment<T>(test: T) -> () where T: FnOnce(&SubroutineEnvironment) -> () + panic::UnwindSafe {
        // setup
        let st1 = SymbolTable::new();
        let mut st2 = SymbolTable::new();
        st2.insert(&Var {
            names: vec!["blargh".to_owned()],
            data_type: Type::Int,
            var_type: VarType::Argument,
        }).unwrap();
        let symbol_table = LayeredSymbolTable::new(&st1, &st2);
        let mut class_subroutines = HashMap::new();
        class_subroutines.insert("someClassMethod".to_owned(), SubroutineType::Method);
        class_subroutines.insert("someClassFunction".to_owned(), SubroutineType::Function);
        class_subroutines.insert("new".to_owned(), SubroutineType::Constructor);
        let environment = SubroutineEnvironment {
            symbol_table,
            class_subroutines: &class_subroutines,
            current_classname: "MyClass".to_owned(),
        };

        let result = panic::catch_unwind(|| {
            test(&environment)
        });

        assert!(result.is_ok())
    }

    #[test]
    fn test_generate_term() {
        run_test_with_environment(|environment| {
            assert_eq!(
                generate_term(&Term::VarName("blargh".to_owned()), environment),
                Ok(vec![VmInstruction::Push(MemorySegment::Argument, 0)])
            );

            assert_eq!(
                generate_term(&Term::Unary(UnaryOp::Not, Box::new(Term::IntegerConstant(2))), environment),
                Ok(vec![VmInstruction::Push(MemorySegment::Constant, 2), VmInstruction::Not])
            );

            assert_eq!(
                generate_term(&Term::KeywordConstant(Keyword::True), environment),
                Ok(vec![
                    VmInstruction::Push(MemorySegment::Constant, 1),
                    VmInstruction::Neg
                ])
            );

            assert_eq!(
                generate_term(&Term::KeywordConstant(Keyword::False), environment),
                Ok(vec![VmInstruction::Push(MemorySegment::Constant, 0)])
            );

            assert!(generate_term(&Term::VarName("argh".to_owned()), environment).is_err());
        });
    }

    #[test]
    fn test_generate_expression() {
        run_test_with_environment(|environment| {
            let expr = Expression(vec![
                ExpressionItem::Term(Term::IntegerConstant(2)),
                ExpressionItem::Operation(BinaryOp::Plus),
                ExpressionItem::Term(Term::IntegerConstant(3)),
            ]);
            let result = generate_expression(&expr, environment);
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
            let longer_expr_result = generate_expression(&longer_expr, environment);
            assert!(longer_expr_result.is_ok());
            assert_eq!(longer_expr_result.unwrap(), vec![
                VmInstruction::Push(MemorySegment::Constant, 2),
                VmInstruction::Push(MemorySegment::Constant, 3),
                VmInstruction::Add,
                VmInstruction::Push(MemorySegment::Constant, 4),
                VmInstruction::Add,
            ]);
        });
    }

    #[test]
    fn test_generate_parenthetical_expr() {
        run_test_with_environment(|environment| {

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

            let result = generate_expression(&expr, environment);
            assert!(result.is_ok());
            assert_eq!(result.unwrap(), vec![
                VmInstruction::Push(MemorySegment::Constant, 2),
                VmInstruction::Push(MemorySegment::Constant, 3),
                VmInstruction::Push(MemorySegment::Constant, 4),
                VmInstruction::Add,
                VmInstruction::Call("Math.multiply".to_owned(), 2),
            ]);
        });
    }
}
