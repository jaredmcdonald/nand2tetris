use rand::random;
use std::fmt;
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
    UnexpectedKeywordConstant,
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

impl fmt::Display for MemorySegment {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::MemorySegment::*;
        write!(f, "{}", match *self {
            Constant => "constant",
            Argument => "argument",
            Local => "local",
            Static => "static",
            This => "this",
            That => "that",
            Pointer => "pointer",
            Temp => "temp",
        })
    }
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
    Return,
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

impl fmt::Display for VmInstruction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use self::VmInstruction::*;
        match *self {
            Push(ref segment, index) => write!(f, "push {} {}", segment, index),
            Pop(ref segment, index) => write!(f, "pop {} {}", segment, index),
            Goto(ref label) => write!(f, "goto {}", label),
            IfGoto(ref label) => write!(f, "if-goto {}", label),
            Label(ref label) => write!(f, "label {}", label),
            Function(ref name, local_count) => write!(f, "function {} {}", name, local_count),
            Call(ref name, num_args) => write!(f, "call {} {}", name, num_args),
            Return => write!(f, "return"),
            Add => write!(f, "add"),
            Sub => write!(f, "sub"),
            And => write!(f, "and"),
            Or => write!(f, "or"),
            Lt => write!(f, "lt"),
            Gt => write!(f, "gt"),
            Eq => write!(f, "eq"),
            Not => write!(f, "not"),
            Neg => write!(f, "neg"),
        }
    }
}

fn generate_string_constant(string: &str) -> Vec<VmInstruction> {
    let len = string.len();
    let mut result = vec![
        VmInstruction::Push(MemorySegment::Constant, len),
        VmInstruction::Call("String.new".to_owned(), 1),
    ];
    for byte in string.as_bytes() {
        result.push(VmInstruction::Push(MemorySegment::Constant, *byte as usize));
        result.push(VmInstruction::Call("String.appendChar".to_owned(), 2));
    }
    result
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
            Keyword::This => vec![VmInstruction::Push(MemorySegment::Pointer, 0)],
            Keyword::Null|Keyword::False => vec![VmInstruction::Push(MemorySegment::Constant, 0)],
            _ => return Err(CodeGenError::UnexpectedKeywordConstant),
        }),
        Term::Unary(op, ref term) => {
            // postfix: -3 -> 3 neg
            let mut result = generate_term(term, environment)?;
            result.push(VmInstruction::from(op));
            Ok(result)
        },
        Term::SubroutineCall(ref s) => generate_subroutine_call(s, environment),
        Term::Parenthetical(ref e) => generate_expression(e, environment),
        Term::IndexExpr(ref identifier, ref index_expr) => {
            let mut result = generate_expression(index_expr, environment)?;
            let (target, index, _) = environment.symbol_table.get(identifier).ok_or(CodeGenError::UnknownVarName)?;
            result.push(VmInstruction::Push(MemorySegment::from(target), index));
            result.push(VmInstruction::Add);
            // `pointer 1` now contains the address of `identifier[index_expr]`, value accessed via `this 0`
            result.push(VmInstruction::Pop(MemorySegment::Pointer, 1)); // get the right address
            result.push(VmInstruction::Push(MemorySegment::That, 0));   // dereference & push
            Ok(result)
        },
        Term::StringConstant(ref s) => Ok(generate_string_constant(s)),
    }
}

// infix:   `a + b + c + d`
// postfix: `a b + c + d +`
fn generate_expression(
    expression: &Expression,
    environment: &SubroutineEnvironment
) -> CodeGenResult {
    let mut expression_iter = expression.clone().into_iter(); // clone 👎
    let mut result = vec![];
    if let Some(ExpressionItem::Term(term1)) = expression_iter.next() {
        result.extend(generate_term(&term1, environment)?);
    } else {
        return Ok(result); // empty expression, can happen e.g. in return statements
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
        let mut result = generate_expression(index_expr, environment)?;
        result.push(VmInstruction::Push(MemorySegment::from(target), index));
        result.push(VmInstruction::Add);
        result.extend(rhs_expression);
        result.push(VmInstruction::Pop(MemorySegment::Temp, 0)); // save the right-hand value on `temp 0`
        // `pointer 1` now contains the address of `identifier[index_expr]`, value accessed via `this 0`
        result.push(VmInstruction::Pop(MemorySegment::Pointer, 1)); // get the right address
        result.push(VmInstruction::Push(MemorySegment::Temp, 0)); // rhs expression value
        result.push(VmInstruction::Pop(MemorySegment::That, 0));  // target value is the next thing on the stack, pop to it
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
    // if condition is false, hop over everything
    result.push(VmInstruction::Not);
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
            result.push(VmInstruction::Push(MemorySegment::Pointer, 0));
            num_args += 1;
        }
        parent_name.push_str(&environment.current_classname);
    }

    for param in &subroutine_call.parameters {
        result.extend(generate_expression(&param, environment)?)
    }

    let name = format!("{}.{}", parent_name, subroutine_call.subroutine_name);
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

fn generate_return_statement(
    expression: &Expression,
    environment: &SubroutineEnvironment
) -> CodeGenResult {
    let mut result = if expression.len() == 0 {
        vec![VmInstruction::Push(MemorySegment::Constant, 0)] // void, per spec we return constant 0
    } else {
        generate_expression(expression, environment)?
    };
    result.push(VmInstruction::Return);
    Ok(result)
}

fn generate_statement(
    statement: &Statement,
    environment: &SubroutineEnvironment
) -> CodeGenResult {
    match *statement {
        Statement::Let(ref statement) => generate_let_statement(statement, environment),
        Statement::If(ref statement) => generate_if_statement(statement, environment),
        Statement::While(ref statement) => generate_while_statement(statement, environment),
        Statement::Do(ref subroutine_call) => generate_do_statement(subroutine_call, environment),
        Statement::Return(ref expression) => generate_return_statement(expression, environment),
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
    field_count: usize,
    class_symbol_table: &SymbolTable,
    class_subroutines: &HashMap<String, SubroutineType>
) -> CodeGenResult {
    let initial_argument_count = if subroutine.subroutine_type == SubroutineType::Method { 1 } else { 0 };
    let mut subroutine_symbol_table = SymbolTable::new(initial_argument_count);
    subroutine_symbol_table.insert_many(&subroutine.params)?;
    subroutine_symbol_table.insert_many(&subroutine.body.var_declarations)?;
    let symbol_table = LayeredSymbolTable::new(class_symbol_table, &subroutine_symbol_table);
    let environment = SubroutineEnvironment {
        symbol_table,
        class_subroutines,
        current_classname: classname.to_owned(),
    };
    let local_count = subroutine.body.var_declarations.iter().fold(0, |sum, val| sum + val.names.len());
    let function_name = format!("{}.{}", classname, &subroutine.name);
    let mut result = vec![VmInstruction::Function(function_name, local_count)];

    if subroutine.subroutine_type == SubroutineType::Constructor {
        // allocate space for fields in constructor
        result.push(VmInstruction::Push(MemorySegment::Constant, field_count));
        result.push(VmInstruction::Call("Memory.alloc".to_owned(), 1));
        result.push(VmInstruction::Pop(MemorySegment::Pointer, 0));
    } else if subroutine.subroutine_type == SubroutineType::Method {
        // set the `this` segment correctly for methods (recieved as argument 0)
        result.push(VmInstruction::Push(MemorySegment::Argument, 0));
        result.push(VmInstruction::Pop(MemorySegment::Pointer, 0));
    }

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
    let mut class_symbol_table = SymbolTable::new(0);
    let (class_vars, subroutines): (Vec<_>, Vec<_>) = class.body.iter().partition(|i| {
        if let ClassBodyItem::ClassVar(_) = **i { true } else { false }
    });
    let mut field_count = 0;
    // can i just partition into iterators instead of the intermediate vecs?
    let mut class_vars_iter = class_vars.iter();
    while let Some(&&ClassBodyItem::ClassVar(ref var)) = class_vars_iter.next() {
        class_symbol_table.insert(&var)?;
        if var.var_type == VarType::Field {
            field_count += var.names.len();
        }
    }

    let mut generated = vec![];
    let class_subroutines = get_subroutine_map(subroutines.as_slice());
    let mut subroutines_iter = subroutines.iter();
    while let Some(&&ClassBodyItem::Subroutine(ref subroutine)) = subroutines_iter.next() {
        generated.extend(generate_subroutine(subroutine, &class.name, field_count, &class_symbol_table, &class_subroutines)?)
    }
    Ok(generated)
}

#[cfg(test)]
mod test {
    use std::panic;
    use super::*;
    use super::VmInstruction::*;
    use super::MemorySegment::*;

    // credit to https://medium.com/@ericdreichert/test-setup-and-teardown-in-rust-without-a-framework-ba32d97aa5ab
    fn run_test_with_environment<T>(initial_argument_count: usize, test: T) ->
            () where T: FnOnce(&SubroutineEnvironment) -> () + panic::UnwindSafe {
        // setup
        let st1 = SymbolTable::new(0);
        let mut st2 = SymbolTable::new(initial_argument_count);
        st2.insert(&Var {
            names: vec!["blargh".to_owned()],
            data_type: Type::Int,
            var_type: VarType::Argument,
        }).unwrap();
        st2.insert(&Var {
            names: vec!["someLocalVariable".to_owned()],
            data_type: Type::Class("SomeOtherClass".to_owned()),
            var_type: VarType::Local,
        }).unwrap();
        st2.insert(&Var {
            names: vec!["anArray".to_owned()],
            data_type: Type::Class("Array".to_owned()),
            var_type: VarType::Local,
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
        run_test_with_environment(0, |environment| {
            assert_eq!(
                generate_term(&Term::VarName("blargh".to_owned()), environment),
                Ok(vec![Push(Argument, 0)])
            );

            assert_eq!(
                generate_term(&Term::Unary(UnaryOp::Not, Box::new(Term::IntegerConstant(2))), environment),
                Ok(vec![Push(Constant, 2), Not])
            );

            assert_eq!(
                generate_term(&Term::KeywordConstant(Keyword::True), environment),
                Ok(vec![
                    Push(Constant, 1),
                    Neg
                ])
            );

            assert_eq!(
                generate_term(&Term::KeywordConstant(Keyword::False), environment),
                Ok(vec![Push(Constant, 0)])
            );

            assert!(generate_term(&Term::VarName("argh".to_owned()), environment).is_err());
        });
    }

    #[test]
    fn test_generate_expression() {
        run_test_with_environment(0, |environment| {
            let expr = Expression(vec![
                ExpressionItem::Term(Term::IntegerConstant(2)),
                ExpressionItem::Operation(BinaryOp::Plus),
                ExpressionItem::Term(Term::IntegerConstant(3)),
            ]);
            let result = generate_expression(&expr, environment);
            assert!(result.is_ok());
            assert_eq!(result.unwrap(), vec![
                Push(Constant, 2),
                Push(Constant, 3),
                Add,
            ]);

            let longer_expr = Expression(vec![
                ExpressionItem::Term(Term::IntegerConstant(2)),
                ExpressionItem::Operation(BinaryOp::Plus),
                ExpressionItem::Term(Term::IntegerConstant(3)),
                ExpressionItem::Operation(BinaryOp::Plus),
                ExpressionItem::Term(Term::IntegerConstant(4)),
            ]);
            let longer_expr_result = generate_expression(&longer_expr, environment);
            assert_eq!(longer_expr_result, Ok(vec![
                Push(Constant, 2),
                Push(Constant, 3),
                Add,
                Push(Constant, 4),
                Add,
            ]));
        });
    }

    #[test]
    fn test_generate_parenthetical_expr() {
        run_test_with_environment(0, |environment| {

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
            assert_eq!(result, Ok(vec![
                Push(Constant, 2),
                Push(Constant, 3),
                Push(Constant, 4),
                Add,
                Call("Math.multiply".to_owned(), 2),
            ]));
        });
    }

    #[test]
    fn test_generate_subroutine_call() {
        run_test_with_environment(0, |environment| {
            assert_eq!(generate_subroutine_call(&SubroutineCall {
                parent_name: Some("someLocalVariable".to_owned()),
                subroutine_name: "someMethod".to_owned(),
                parameters: vec![Expression(vec![ExpressionItem::Term(Term::IntegerConstant(1))])],
            }, environment), Ok(vec![
                Push(Local, 0),
                Push(Constant, 1),
                Call("SomeOtherClass.someMethod".to_owned(), 2)
            ]));

            assert_eq!(generate_subroutine_call(&SubroutineCall {
                parent_name: None,
                subroutine_name: "someClassMethod".to_owned(),
                parameters: vec![Expression(vec![ExpressionItem::Term(Term::IntegerConstant(1))])],
            }, environment), Ok(vec![
                Push(Pointer, 0),
                Push(Constant, 1),
                Call("MyClass.someClassMethod".to_owned(), 2)
            ]));
        });
    }

    #[test]
    fn test_generate_let_statement() {
        run_test_with_environment(0, |environment| {
            let generated = generate_let_statement(
                // let anArray[1] = anArray[1 + 1];
                &LetStatement {
                    index_expression: Some(Expression(vec![
                        ExpressionItem::Term(Term::IntegerConstant(1))
                    ])),
                    name: "anArray".to_owned(),
                    expression: Expression(vec![
                        ExpressionItem::Term(Term::IntegerConstant(1)),
                        ExpressionItem::Operation(BinaryOp::Plus),
                        ExpressionItem::Term(Term::IntegerConstant(1))
                    ])
                },
                environment
            );
            assert_eq!(generated, Ok(vec![
                Push(Constant, 1),
                Push(Local, 1),
                Add,
                Push(Constant, 1),
                Push(Constant, 1),
                Add,
                Pop(Temp, 0),
                Pop(Pointer, 1),
                Push(Temp, 0),
                Pop(That, 0)
            ]));
        });
    }
}
