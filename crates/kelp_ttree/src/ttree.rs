pub struct Lambda {
    args: Vec<(Symbol, Type)>,
    ret: Type,
    body: Vec<Variable>,
}

pub struct Expression {
    lhs: Variable,
    op: Symbol,
    rhs: Variable,
}

pub struct Variable {
    
}