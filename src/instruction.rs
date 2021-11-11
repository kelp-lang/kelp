use std::{cell::RefCell, collections::HashMap, fmt::Display, rc::Rc};

use crate::{span::Span};

#[derive(Debug, Clone)]
pub struct Instruction {
  pub inner: Rc<RefCell<InstructionInner>>,
  span: Span,
}

impl Instruction {
  pub fn new(inner: InstructionInner, span: Span) -> Instruction {
    Self {
      inner: Rc::new(RefCell::new(inner)),
      span,
    }
  }

  pub fn inner(&self) -> InstructionInner {
    (*self.inner.borrow()).clone()
  }

  pub fn span(&self) -> Span {
    self.span.clone()
  }

  pub fn shorten_seq(&self, front_by: Option<usize>, back_by: Option<usize>) -> Self {
    if let InstructionInner::SequenceStructure(seq) = self.inner() {
      Self {
        inner: Rc::new(RefCell::new(InstructionInner::SequenceStructure(seq[front_by.unwrap_or(0)..back_by.unwrap_or(seq.len() - 1)].to_vec()))),
        span: self.span.clone()
      }
    } else {
      let span = self.span.clone();
      error!(span, "Cannot shorten, as it is not a sequence");
      Default::default()
    }
  }
}

#[derive(Debug, Clone)]
pub enum Literal {
  Int(i64),
  Float(f32),
  String(String),
  True,
  False,
  Empty
}

#[derive(Debug, Clone)]
pub enum InstructionInner {
  DefCall {
    symbol: String,
    instruction: Instruction,
  },
  GetCall(String),
  Symbol(String),
  FunctionDefinition {
    params: Vec<String>,
    body: Instruction,
  },
  MacroDefinition {
    params: Vec<String>,
    body: Instruction,
  },
  FunctionCall {
    symbol: String,
    arguments: Vec<Instruction>,
    env_id: usize,
  },
  Closure {
    binds: HashMap<String, Instruction>,
    body: Instruction,
  },
  SequenceStructure(Vec<Instruction>),
  ListStructure(Vec<Instruction>),
  LiteralStructure(Literal),
  FromFuncall,
  Quote(Instruction),
  Eval(Instruction),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
       match self.inner() {
        InstructionInner::DefCall { symbol, instruction } => write!(f, "define {} as {}", symbol, instruction),
        InstructionInner::GetCall(symbol) => write!(f, "get {}", symbol),
        InstructionInner::FunctionDefinition { params, body } => write!(f, "function [{}] {}", params.iter().map(|p| p.to_string()).reduce(|acc, p| format!("{}, {}", acc, p)).unwrap_or_else(|| "".to_string()), body),
        InstructionInner::MacroDefinition { params, body } => write!(f, "macro [{}] {}", params.iter().map(|p| p.to_string()).reduce(|acc, p| format!("{}, {}", acc, p)).unwrap_or_else(|| "".to_string()), body),
        InstructionInner::FunctionCall { symbol, arguments, .. } => write!(f, "call {} with [{}]", symbol, arguments.iter().map(|a| a.to_string()).reduce(|acc, a| format!("{}, {}", acc, a)).unwrap_or_else(|| "".to_string())),
        InstructionInner::Closure { .. } => todo!(),
        InstructionInner::SequenceStructure(seq) => write!(f, "({})", seq.iter().map(|s| s.to_string()).reduce(|acc, a| format!("{} {}", acc, a)).unwrap_or_else(|| "".to_string())),
        InstructionInner::LiteralStructure(lit) => match lit {
            Literal::Int(i) => write!(f, "{}", i),
            Literal::Float(fl) => write!(f, "{}", fl),
            Literal::String(s) => write!(f, "\"{}\"", s),
            Literal::True => write!(f, "True"),
            Literal::False => write!(f, "False"),
            Literal::Empty => write!(f, "Empty"),
        },
        InstructionInner::Symbol(s) => write!(f, "symbol {}", s),
        InstructionInner::FromFuncall => write!(f, "from funcall"),
        InstructionInner::ListStructure(seq) => write!(f, "({})", seq.iter().map(|s| s.to_string()).reduce(|acc, a| format!("{}\n  {}", acc, a)).unwrap_or_else(|| "".to_string())),
        InstructionInner::Quote(s) => write!(f, "'{}", s),
        InstructionInner::Eval(s) => write!(f, "eval {}", s),
    } 
    }
}

impl Default for Instruction {
    fn default() -> Self {
        Self { inner: Rc::new(RefCell::new(InstructionInner::LiteralStructure(Literal::Empty))), span: Default::default() }
    }
}