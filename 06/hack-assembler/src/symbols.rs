use std::collections::HashMap;
use std::collections::hash_map::Entry;

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
    pub fn set(&mut self, label: &str, addr: u16) {
        self.table.insert(label.to_string(), addr);
    }

    // useful for nonlabel symbols
    pub fn get_else_set(&mut self, label: &str) -> u16 {
        let addr = match self.table.entry(label.to_string()) {
            Entry::Occupied(entry) => entry.get() * 1,
            Entry::Vacant(entry) => {
                entry.insert(self.current);
                self.current
            },
        };
        self.current += 1;
        addr
    }
}
