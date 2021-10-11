#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ErrorType {
    Definition,
    TupleKey,
    NotSequence,
    InvalidType,
    MacroDefinition
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum BitWidth {
    Eight = 8,
    Sixteen = 16,
    ThirtyTwo = 32,
    SixtyFour = 64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Signed {
    Signed = 0,
    Unsigned = 1,
}

#[derive(Debug, Clone, Eq)]
pub enum Type {
    Int(BitWidth, Signed),
    Float(BitWidth, Signed),
    String,
    Bool,
    Function {
        params: Vec<Type>,
        ret: Box<Type>
    },
    Macro {
        params: Vec<Type>,
        ret: Box<Type>
    },
    Symbol,
    Sequence(Vec<Type>),
    // TODO: Arrays and lists
    List(Vec<Type>),
    Vector(Box<Type>),
    Tuple(Vec<(String, Type)>),
    Quote(Box<Type>),
    Unquote(Box<Type>),
    Quasiquote(Box<Type>),
    Eval(Box<Type>),
    Empty,
    Unknown,
    Error(ErrorType)
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(l0, l1), Self::Int(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Float(l0, l1), Self::Float(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Function { params: l_params, ret: l_ret }, Self::Function { params: r_params, ret: r_ret }) => l_params == r_params && l_ret == r_ret,
            (Self::Macro { params: l_params, ret: l_ret }, Self::Macro { params: r_params, ret: r_ret }) => l_params == r_params && l_ret == r_ret,
            (Self::Sequence(l0), Self::Sequence(r0)) => l0 == r0,
            (Self::List(l0), Self::List(r0)) => l0 == r0,
            (Self::Tuple(l0), Self::Tuple(r0)) => l0.iter().all(|l| r0.contains(l)),
            (Self::Quote(l0), Self::Quote(r0)) => l0 == r0,
            (Self::Unquote(l0), Self::Unquote(r0)) => l0 == r0,
            (Self::Quasiquote(l0), Self::Quasiquote(r0)) => l0 == r0,
            (Self::Eval(l0), Self::Eval(r0)) => l0 == r0,
            _ => todo!(),
        }
    }
}

pub trait Typed {
    fn typ(&self) -> Type;
}