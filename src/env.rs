use std::{collections::HashMap};

use crate::instruction::Instruction;

#[derive(Debug, Clone)]
pub struct EnvironmentStore {
  envs: HashMap<usize, Environment>,
  id_counter: usize,
}

impl EnvironmentStore {
  pub fn new() -> Self {
    Self {
      envs: HashMap::new(),
      id_counter: 0,
    }
  }
  pub fn new_env(&mut self, outer: Option<usize>) -> usize {
    self.id_counter += 1;
    self.envs.insert(self.id_counter, Environment::new(outer));
    self.id_counter
  }

  pub fn set_at(&mut self, id: usize, symbol: String, value: Instruction) -> Option<Instruction> {
    self.envs.get_mut(&id).unwrap().symbol_map.insert(symbol, value)
  }

  pub fn get_at(&self, id: usize, symbol: &str) -> Option<Instruction> {
    self.envs[&id].symbol_map.get(symbol).cloned()
  }
}

impl Default for EnvironmentStore {
    fn default() -> Self {
        Self::new()
    }
}


#[derive(Debug, Clone)]
pub struct Environment {
    symbol_map: HashMap<String, Instruction>,
    outer: Option<usize>,
}

impl Environment {
    pub fn new(outer: Option<usize>) -> Environment {
        Self {
            symbol_map: HashMap::new(),
            outer,
        }
    }

    pub fn get(&self, symbol: &str) -> Option<Instruction> {
        self.symbol_map.get(symbol).cloned()
    }

    pub fn set(&mut self, symbol: String, value: Instruction) -> Option<Instruction> {
        self.symbol_map.insert(symbol, value)
    }
}
