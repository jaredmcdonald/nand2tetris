use std::collections::HashMap;
use std::collections::hash_map::Entry;
use ast::{Var, VarType, Type};

#[derive(Debug, PartialEq)]
pub enum SymbolError {
    Occupied,
}

#[derive(Debug, PartialEq)]
pub struct SymbolTable {
    table: HashMap<String, (Var, usize)>,
    static_counter: usize,
    field_counter: usize,
    argument_counter: usize,
    local_counter: usize,
}

#[derive(Debug, PartialEq)]
pub struct LayeredSymbolTable<'a> {
    class_symbol_table: &'a SymbolTable,
    subroutine_symbol_table: &'a SymbolTable,
}

impl <'a>LayeredSymbolTable<'a> {

    pub fn new(
        class_symbol_table: &'a SymbolTable,
        subroutine_symbol_table: &'a SymbolTable
    ) -> LayeredSymbolTable<'a> {
        LayeredSymbolTable { class_symbol_table, subroutine_symbol_table }
    }

    pub fn get(&self, name: &str) -> Option<(VarType, usize, Type)> {
        self.subroutine_symbol_table.get(name).or(self.class_symbol_table.get(name))
    }
}

impl SymbolTable {
    pub fn new(initial_argument_counter: usize) -> SymbolTable {
        SymbolTable {
            table: HashMap::new(),
            static_counter: 0,
            field_counter: 0,
            argument_counter: initial_argument_counter,
            local_counter: 0,
        }
    }

    pub fn insert(&mut self, var: &Var) -> Result<(), SymbolError> {
        for name in &var.names {
            if let Entry::Vacant(entry) = self.table.entry(name.to_owned()) {
                entry.insert((var.clone(), match var.var_type {
                    VarType::Static => self.static_counter,
                    VarType::Field => self.field_counter,
                    VarType::Argument => self.argument_counter,
                    VarType::Local => self.local_counter,
                })); // TODO can i dedupe these 👆 👇 somehow?
                match var.var_type {
                    VarType::Static => self.static_counter += 1,
                    VarType::Field => self.field_counter += 1,
                    VarType::Argument => self.argument_counter += 1,
                    VarType::Local => self.local_counter += 1,
                };
            } else {
                return Err(SymbolError::Occupied);
            }
        }
        Ok(())
    }

    pub fn insert_many(&mut self, vars: &[Var]) -> Result<(), SymbolError> {
        for var in vars.iter() {
            self.insert(&var)?;
        }
        Ok(())
    }

    pub fn get(&self, name: &str) -> Option<(VarType, usize, Type)> {
        self.table.get(name).and_then(|result| {
            let &(ref var, index) = result;
            Some((var.var_type, index, var.data_type.clone()))
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use ast::Type;

    #[test]
    fn test_constructor() {
        let mut st = SymbolTable::new(1);
        assert_eq!(st.argument_counter, 1);
        st.insert(&Var {
            names: vec!["blargh".to_owned()],
            data_type: Type::Int,
            var_type: VarType::Argument,
        }).unwrap();
        let (_, index, _) = st.get("blargh").unwrap();
        assert_eq!(index, 1);
    }

    #[test]
    fn test_insert() {
        let mut st = SymbolTable::new(0);

        assert!(st.insert(&Var {
            names: vec!["blargh".to_owned()],
            data_type: Type::Int,
            var_type: VarType::Static,
        }).is_ok());

        assert_eq!(st.insert(&Var {
            names: vec!["argh".to_owned(), "blargh".to_owned()],
            data_type: Type::Class("MyClass".to_owned()),
            var_type: VarType::Field,
        }), Err(SymbolError::Occupied));
    }

    #[test]
    fn test_insert_many() {
        let mut st = SymbolTable::new(0);

        assert!(st.insert_many(&[
            Var {
                names: vec!["blargh".to_owned()],
                data_type: Type::Int,
                var_type: VarType::Static,
            },
            Var {
                names: vec!["argh".to_owned()],
                data_type: Type::Int,
                var_type: VarType::Field,
            }
        ]).is_ok());

        assert_eq!(st.insert_many(&[
            Var {
                names: vec!["blargh1".to_owned()],
                data_type: Type::Int,
                var_type: VarType::Static,
            },
            Var {
                names: vec!["argh".to_owned()],
                data_type: Type::Int,
                var_type: VarType::Field,
            }
        ]), Err(SymbolError::Occupied));
    }

    #[test]
    fn test_get() {
        let mut st = SymbolTable::new(0);
        let var = Var {
            names: vec!["argh".to_owned(), "blargh".to_owned()],
            data_type: Type::Class("MyClass".to_owned()),
            var_type: VarType::Argument,
        };
        st.insert(&var).unwrap();
        assert_eq!(st.get("argh"), Some((VarType::Argument, 0, Type::Class("MyClass".to_owned()))));
        assert_eq!(st.get("blargh"), Some((VarType::Argument, 1, Type::Class("MyClass".to_owned()))));
        assert_eq!(st.get("arghblargh"), None);
    }

    #[test]
    fn test_layered_get() {
        let mut t1 = SymbolTable::new(0);
        t1.insert(&Var {
            names: vec!["argh".to_owned(), "blargh".to_owned()],
            data_type: Type::Class("MyClass".to_owned()),
            var_type: VarType::Argument,
        }).unwrap();
        let mut t2 = SymbolTable::new(0);
        t2.insert(&Var {
            names: vec!["blargh1".to_owned()],
            data_type: Type::Int,
            var_type: VarType::Static,
        }).unwrap();

        let layered = LayeredSymbolTable::new(&t2, &t1);

        assert_eq!(layered.get("blargh"), Some((VarType::Argument, 1, Type::Class("MyClass".to_owned()))));
        assert_eq!(layered.get("blargh1"), Some((VarType::Static, 0, Type::Int)));
        assert_eq!(layered.get("blargh3"), None);
    }
}
