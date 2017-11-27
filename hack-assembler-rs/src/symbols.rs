use std::collections::HashMap;
use std::collections::hash_map::Entry;

#[derive(Clone)]
pub struct SymbolTable {
    table: HashMap<String, u16>,
    current: u16,
}

impl SymbolTable {
    pub fn new() -> SymbolTable {
        let mut table = HashMap::new();
        table.insert("SP".to_string(),     0x0);
        table.insert("LCL".to_string(),    0x1);
        table.insert("ARG".to_string(),    0x2);
        table.insert("THIS".to_string(),   0x3);
        table.insert("THAT".to_string(),   0x4);
        table.insert("R0".to_string(),     0x0);
        table.insert("R1".to_string(),     0x1);
        table.insert("R2".to_string(),     0x2);
        table.insert("R3".to_string(),     0x3);
        table.insert("R4".to_string(),     0x4);
        table.insert("R5".to_string(),     0x5);
        table.insert("R6".to_string(),     0x6);
        table.insert("R7".to_string(),     0x7);
        table.insert("R8".to_string(),     0x8);
        table.insert("R9".to_string(),     0x9);
        table.insert("R10".to_string(),    0xa);
        table.insert("R11".to_string(),    0xb);
        table.insert("R12".to_string(),    0xc);
        table.insert("R13".to_string(),    0xd);
        table.insert("R14".to_string(),    0xe);
        table.insert("R15".to_string(),    0xf);
        table.insert("SCREEN".to_string(), 0x4000);
        table.insert("KBD".to_string(),    0x6000);
        SymbolTable { table, current: 0x10 }
    }

    // for setting labels on first pass
    pub fn set(&mut self, label: &str, addr: u16) -> Result<u16, &str> {
        if let Entry::Vacant(entry) = self.table.entry(label.to_string()) {
            entry.insert(addr);
            Ok(addr)
        } else {
            Err("occupied")
        }
    }

    // for nonlabel symbols
    pub fn get_else_set(&mut self, label: &str) -> u16 {
        let addr = match self.table.entry(label.to_string()) {
            Entry::Occupied(entry) => entry.get() * 1, // todo: how to get rid of borrowing issue without this hack?
            Entry::Vacant(entry) => {
                let current = self.current;
                entry.insert(current);
                self.current += 1;
                current
            },
        };
        addr
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_constructor() {
        let mut t = SymbolTable::new();
        assert_eq!(t.get_else_set("SP"), 0);
        assert_eq!(t.get_else_set("SCREEN"), 0x4000);
        assert_eq!(t.get_else_set("R5"), 0x5);
    }

    #[test]
    fn test_get_else_set() {
        let mut t = SymbolTable::new();
        assert_eq!(t.get_else_set("blargh"), 0x10);
        assert_eq!(t.get_else_set("blargh"), 0x10);
        assert_eq!(t.get_else_set("blargh1"), 0x11);
        assert_eq!(t.get_else_set("blargh2"), 0x12);
    }

    #[test]
    fn test_set() {
        let mut t = SymbolTable::new();
        if let Err(_) = t.set("BLARGH", 0xa) {
            panic!("setting a new label should succeed");
        };
        assert_eq!(t.get_else_set("BLARGH"), 0xa);
    }

    #[test]
    fn test_set_occupied() {
        let mut t = SymbolTable::new();
        if let Err(_) = t.set("BLARGH", 0xa) {
            panic!("setting a new label should succeed");
        };
        if let Ok(_) = t.set("BLARGH", 0xb) {
            panic!("setting an existing label should fail");
        };
        assert_eq!(t.get_else_set("BLARGH"), 0xa);
    }
}
