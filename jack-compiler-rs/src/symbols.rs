use std::collections::HashMap;
use std::collections::hash_map::Entry;
use parse::{Var, VarType};

#[derive(Debug, PartialEq)]
pub enum SymbolError {
    Occupied,
    NotFound
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

    pub fn get(&self, name: &str) -> Result<(VarType, usize), SymbolError> {
        if let Ok(result) = self.subroutine_symbol_table.get(name) {
            Ok(result)
        } else {
            Ok(self.class_symbol_table.get(name)?)
        }
    }
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        SymbolTable {
            table: HashMap::new(),
            static_counter: 0,
            field_counter: 0,
            argument_counter: 0,
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

    pub fn get(&self, name: &str) -> Result<(VarType, usize), SymbolError> {
        let &(ref var, index) = self.table.get(name).ok_or(SymbolError::NotFound)?;
        Ok((var.var_type, index))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use parse::Type;

    #[test]
    fn test_insert() {
        let mut st = SymbolTable::new();

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
        let mut st = SymbolTable::new();

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
    fn test_get_method() {
        let mut st = SymbolTable::new();
        let var = Var {
            names: vec!["argh".to_owned(), "blargh".to_owned()],
            data_type: Type::Class("MyClass".to_owned()),
            var_type: VarType::Argument,
        };
        st.insert(&var).unwrap();
        assert_eq!(st.get("argh").unwrap(), (VarType::Argument, 0));
        assert_eq!(st.get("blargh").unwrap(), (VarType::Argument, 1));

        assert_eq!(st.get("arghblargh"), Err(SymbolError::NotFound));
    }

    #[test]
    fn test_layered_get() {
        let mut t1 = SymbolTable::new();
        t1.insert(&Var {
            names: vec!["argh".to_owned(), "blargh".to_owned()],
            data_type: Type::Class("MyClass".to_owned()),
            var_type: VarType::Argument,
        }).unwrap();
        let mut t2 = SymbolTable::new();
        t2.insert(&Var {
            names: vec!["blargh1".to_owned()],
            data_type: Type::Int,
            var_type: VarType::Static,
        }).unwrap();

        let layered = LayeredSymbolTable::new(&t2, &t1);

        assert_eq!(layered.get("blargh").unwrap(), (VarType::Argument, 1));
        assert_eq!(layered.get("blargh1").unwrap(), (VarType::Static, 0));
        assert_eq!(layered.get("blargh3"), Err(SymbolError::NotFound));
    }
}
