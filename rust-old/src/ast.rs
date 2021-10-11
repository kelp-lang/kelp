use std::{
    cell::{Ref, RefCell},
    fmt::Display,
    rc::Rc,
};

use indexmap::IndexMap;

use crate::{
    context::ContextStore,
    typer::KelpType,
    types::{KelpVal, _KelpVal},
};

#[derive(Clone)]
pub struct Span {
    pub start: usize,
    pub end: usize,
    pub content: String,
    ln: usize,
    col: usize,
    line_start: usize,
    line_end: usize,
    pub props_generated: bool,
}

impl Span {
    pub fn ln(&mut self) -> usize {
        self.calculate_props();

        self.ln
    }

    pub fn col(&mut self) -> usize {
        self.calculate_props();

        self.col
    }

    pub fn line_start(&mut self) -> usize {
        self.calculate_props();

        self.line_start
    }

    pub fn line_end(&mut self) -> usize {
        self.calculate_props();

        self.line_end
    }

    fn calculate_props(&mut self) {
        if self.props_generated {
            return;
        }
        let mut line_count = 0;
        let mut last_lb_pos = 0;

        for (i, c) in self.content.chars().enumerate() {
            if c == '\n' {
                last_lb_pos = i;
                line_count += 1;
            }

            if i == self.start {
                self.line_start = last_lb_pos;
                self.ln = line_count;
                self.col = i - self.line_start;

                for (i, c) in self.content[self.end..].chars().enumerate() {
                    if c == '\n' {
                        self.line_end = i + self.end;
                        self.props_generated = true;
                        return;
                    }
                }
                self.line_end = self.end;
                self.props_generated = true;
                return;
            }
        } 
        println!("just ran out");
    }

    pub fn new(start: usize, end: usize, content: String) -> Span {
        Self {
            start: start,
            end: end,
            content: content,
            ln: 0,
            col: 0,
            line_start: 0,
            line_end: 0,
            props_generated: false,
        }
    }
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let content = &self.content[self.start..self.end];
        write!(f, "{}", content)
    }
}

#[derive(Debug, Clone)]
pub enum _Node {
    Int64(i64),
    Float32(f32),
    Bool(bool),
    String(String),
    Symbol(String),
    Expression {
        op: Node,
        lhs: Node,
        rhs: Node,
    },
    Lambda {
        arg: Node,
        ret: KelpType,
        body: Node,
    },
    Body(Vec<Node>),
    Group(Vec<Node>),
    Enum(IndexMap<String, (usize, Option<Node>)>),
    Empty,
}

#[derive(Debug, Clone)]
pub struct Node {
    context: String,
    inner: Rc<RefCell<_Node>>,
    typ: KelpType,
    span: Span,
}

impl Node {
    pub fn span(&self) -> Span {
        self.span.clone()
    }

    pub fn typ(&self) -> KelpType {
        self.typ.clone()
    }

    pub fn inner(&self) -> Ref<'_, _Node> {
        self.inner.borrow()
    }

    pub fn from_value(value: KelpVal, context_store: &mut ContextStore) -> Self {
        let context = context_store.current_context.clone();
        let (inner, typ) = match value.inner() {
            crate::types::_KelpVal::Int64(i) => (
                _Node::Int64(i.clone()),
                KelpType::Type {
                    name: "Int".to_string(),
                    value: Rc::new(KelpType::Int),
                },
            ),
            crate::types::_KelpVal::Float32(f) => (
                _Node::Float32(f.clone()),
                KelpType::Type {
                    name: "Float".to_string(),
                    value: Rc::new(KelpType::Float),
                },
            ),
            crate::types::_KelpVal::Bool(b) => (
                _Node::Bool(b.clone()),
                KelpType::Type {
                    name: "Bool".to_string(),
                    value: Rc::new(KelpType::Bool),
                },
            ),
            crate::types::_KelpVal::Str(s) => (
                _Node::String(s.clone()),
                KelpType::Type {
                    name: "String".to_string(),
                    value: Rc::new(KelpType::String),
                },
            ),
            crate::types::_KelpVal::Sym { name, typ } => (
                _Node::Symbol(name.clone()),
                KelpType::Symbol {
                    name: name.clone(),
                    typ: Rc::new(type_sym(
                        name.clone(),
                        typ.clone(),
                        value.span(),
                        context_store,
                    )),
                },
            ),
            crate::types::_KelpVal::Unknown => (_Node::Empty, KelpType::Unknown),
            crate::types::_KelpVal::Expr { lhs, op, rhs } => typecheck_expr(
                lhs.clone(),
                op.clone(),
                rhs.clone(),
                value.span(),
                context_store,
            ),
            crate::types::_KelpVal::Lambda { arg, body, ret } => {
                typecheck_lambda(arg.clone(), body.clone(), ret.clone(), context_store)
            }
            crate::types::_KelpVal::Body(expressions) => {
                let expression_nodes = expressions
                    .iter()
                    .map(|e| Node::from_value(e.clone(), context_store))
                    .collect::<Vec<_>>();
                let expression_types = expression_nodes.iter().map(|e| e.typ()).collect::<Vec<_>>();

                (
                    _Node::Body(expression_nodes),
                    expression_types.last().unwrap_or(&KelpType::Empty).clone(),
                )
            }
            crate::types::_KelpVal::Group(members) => {
                let member_nodes = members
                    .iter()
                    .map(|m| Node::from_value(m.clone(), context_store))
                    .collect::<Vec<_>>();
                let member_types = member_nodes.iter().map(|m| m.typ()).collect::<Vec<_>>();

                (
                    _Node::Group(member_nodes),
                    KelpType::Group {
                        members: member_types,
                    },
                )
            }
            crate::types::_KelpVal::Enum(_) => todo!(),
            crate::types::_KelpVal::Trait(_) => todo!(),
        };
        Self {
            context: context,
            inner: Rc::new(RefCell::new(inner)),
            typ: typ,
            span: value.span(),
        }
    }
}

fn typecheck_lambda(
    arg: KelpVal,
    body: KelpVal,
    ret: KelpVal,
    context_store: &mut ContextStore,
) -> (_Node, KelpType) {
    context_store.add_child_context();
    let arg_node = {
        let (node, typ) = typecheck_arg(arg.clone(), context_store);

        Node {
            context: context_store.current_context.clone(),
            inner: Rc::new(RefCell::new(node)),
            typ: typ,
            span: arg.span(),
        }
    };

    let body_node = Node::from_value(body.clone(), context_store);

    let ret_type = match ret.inner() {
        _KelpVal::Unknown => {
            let span = ret.span();
            error!(span, "Lambda must have a return type!");
            body_node.typ()
        },
        _ => {
            let ret_node = val_type_to_kelp_type(ret.clone(), context_store);

            if body_node.typ() != ret_node {
                let span = ret.span();
                error!(
                    span,
                    "Declared return type: '{}' does not match the body return type: '{}'",
                    ret_node,
                    body_node.typ()
                );
            }

            ret_node.clone()
        }
    };

    if ret_type != body_node.typ() {
        let span = body_node.span();
        error!(span, "The declared type {} and the real type {} are not the same", ret_type, body_node.typ())
    }
    let _ = context_store.escape_context();

    (
        _Node::Lambda {
            arg: arg_node.clone(),
            body: body_node,
            ret: ret_type.clone(),
        },
        KelpType::Lambda {
            arg: Rc::new(arg_node.typ()),
            ret: Rc::new(ret_type),
        },
    )
}

fn typecheck_arg(arg: KelpVal, context_store: &mut ContextStore) -> (_Node, KelpType) {
    fn typecheck_arg_group(
        members: Vec<KelpVal>,
        context_store: &mut ContextStore,
    ) -> (_Node, KelpType) {
        let member_nodes: Vec<_> = members
            .iter()
            .map(|m| typecheck_arg_member(m.clone(), context_store))
            .collect();
        let member_types = member_nodes.iter().map(|m| m.typ()).collect();

        (
            _Node::Group(member_nodes),
            KelpType::Group {
                members: member_types,
            },
        )
    }
    fn typecheck_arg_sym(
        name: String,
        typ: Option<KelpVal>,
        span: Span,
        context_store: &mut ContextStore,
    ) -> (_Node, KelpType) {
        match typ {
            Some(t) => {
                let typ = val_type_to_kelp_type(t.clone(), context_store);
                context_store.add_to_context(name.clone(), typ.clone());
                (_Node::Symbol(name.clone()), typ)
            }
            None => {
                error!(span, "Lambda argument must have a type");
                (_Node::Symbol(name.clone()), KelpType::Unknown)
            }
        }
    }

    fn typecheck_arg_member(member: KelpVal, context_store: &mut ContextStore) -> Node {
        let span = member.span();

        let (node, typ) = match member.inner() {
            _KelpVal::Sym { name, typ } => {
                typecheck_arg_sym(name.clone(), typ.clone(), span.clone(), context_store)
            }
            _KelpVal::Group(members) => typecheck_arg_group(members.clone(), context_store),
            _ => {
                error!(
                    span,
                    "Argument of an lambda function cannot be of type: `{}`", member
                );
                (_Node::Empty, KelpType::Unknown)
            }
        };
        Node {
            inner: Rc::new(RefCell::new(node)),
            typ: typ,
            span: span,
            context: context_store.current_context.clone(),
        }
    }

    let span = arg.span();
    match arg.inner() {
        _KelpVal::Sym { name, typ } => {
            typecheck_arg_sym(name.clone(), typ.clone(), span, context_store)
        }
        _KelpVal::Lambda { arg, body, ret } => {
            context_store.change_possible_child_context(None);
            typecheck_lambda(arg.clone(), body.clone(), ret.clone(), context_store)
        }
        _KelpVal::Group(members) => typecheck_arg_group(members.clone(), context_store),
        _ => {
            error!(
                span,
                "Argument of an lambda function cannot be of type: '{}'", arg
            );
            (_Node::Empty, KelpType::Unknown)
        }
    }
}

fn typecheck_expr(
    lhs: KelpVal,
    op: KelpVal,
    rhs: KelpVal,
    span: Span,
    context_store: &mut ContextStore,
) -> (_Node, KelpType) {
    if let _KelpVal::Sym {
        name: op_sym,
        typ: _,
    } = op.inner()
    {
        match (lhs.inner(), &op_sym[..]) {
            (
                _KelpVal::Sym {
                    name: lhs_sym,
                    typ: lhs_type,
                },
                "=",
            ) => {
                // Check if symbol doesn't already exist
                if context_store.get_symbol_type(lhs_sym).is_some() {
                    let span = lhs.span();
                    error!(
                        span,
                        "Variables are immutable and {} is already defined", lhs_sym
                    );
                }

                // Create rhs with the new child context
                context_store.change_possible_child_context(Some(lhs_sym.clone()));
                let rhs_node = Node::from_value(rhs, context_store);
                context_store.change_possible_child_context(None);

                // Typecheck the expression
                if let Some(lhs_type) = lhs_type {
                    let val_type = val_type_to_kelp_type(lhs_type.clone(), context_store);
                    if rhs_node.typ() == val_type {
                    } else {
                        error!(
                            span,
                            "The left sides type: '{}' does not match the right sides type: {}",
                            val_type,
                            rhs_node.typ()
                        );
                    }
                }

                // Register the new variable to context
                context_store.add_to_context(lhs_sym.clone(), rhs_node.typ());

                let lhs_node = Node::from_value(lhs, context_store);
                let op_node = Node {
                    context: context_store.current_context.clone(),
                    inner: Rc::new(RefCell::new(_Node::Symbol("=".to_string()))),
                    typ: KelpType::Special,
                    span: op.span(),
                };

                (
                    _Node::Expression {
                        op: op_node,
                        lhs: lhs_node,
                        rhs: rhs_node.clone(),
                    },
                    rhs_node.typ(),
                )
            }
            (_, "->") => {
                let lhs_node = Node::from_value(lhs.clone(), context_store);
                let rhs_node = Node::from_value(rhs.clone(), context_store);

                let expr_type = match rhs_node.inner().clone() {
                    _Node::Lambda { arg, body: _, ret } => {
                        if arg.typ() != lhs_node.typ() {
                            error!(
                                span,
                                "The lambda: '{}' doesn't take arguments that were provided: {}",
                                rhs,
                                lhs
                            );
                        }
                        ret
                    }
                    _Node::Symbol(s) => match context_store.get_symbol_type(&s) {
                        Some(KelpType::Lambda { arg, ret }) => {
                            if *arg != lhs_node.typ() {
                                error!(span, "The function: '{}' doesn't take arguments that were provided: {}", rhs_node, lhs_node);
                            }

                            (*(ret.clone())).clone()
                        }
                        _ => {
                            error!(span, "The symbol '{}' is not a function", s);
                            KelpType::Unknown
                        }
                    },
                    _ => {
                        error!(span, "'{}' is not a function nor a lambda", rhs);
                        KelpType::Unknown
                    }
                };

                (
                    _Node::Expression {
                        op: Node {
                            context: context_store.current_context.clone(),
                            inner: Rc::new(RefCell::new(_Node::Symbol(">".to_string()))),
                            typ: KelpType::Special,
                            span: span,
                        },
                        lhs: lhs_node,
                        rhs: rhs_node,
                    },
                    expr_type,
                )
            }
            _ => {
                //println!("op: {}", op);
                let lhs_node = Node::from_value(lhs.clone(), context_store);
                let rhs_node = Node::from_value(rhs.clone(), context_store);
                //println!("{}", op);
                let op_node = Node::from_value(op.clone(), context_store);

                //println!("lhs typ: {}", lhs_node.typ());
                let args = KelpType::Group {
                    members: vec![lhs_node.typ(), rhs_node.typ()],
                };
                let ret = match context_store.get_symbol_type(op_sym) {
                    Some(op_type) => match op_type.clone() {
                        KelpType::Lambda { arg: op_args, ret } => {
                            if args != *op_args {
                                error!(span, "Operator arguments '{}' and the left {} and right {} side don't match", args, lhs, rhs);
                            }
                            (*(ret.clone())).clone()
                        }
                        _ => {
                            error!(span, "Symbol '{}' is not an operator", op.clone());
                            KelpType::Unknown
                        }
                    },
                    None => {
                        error!(span, "Operator '{}' does not exist", op_sym);
                        KelpType::Unknown
                    }
                };

                (
                    _Node::Expression {
                        lhs: lhs_node,
                        rhs: rhs_node,
                        op: op_node,
                    },
                    ret,
                )
            }
        }
    } else {
        let span = op.span();
        error!(span, "{} is not a valid operator", op);

        (_Node::Empty, KelpType::Unknown)
    }
}

fn type_sym(
    name: String,
    typ: Option<KelpVal>,
    span: Span,
    context_store: &mut ContextStore,
) -> KelpType {
    match (typ, context_store.get_symbol_type(&name)) {
        (None, None) => {
            error!(span, "{} has no type and type cannot be determined", name);
            KelpType::Unknown
        }
        (None, Some(t_sym)) => t_sym,
        (Some(t_val), None) => val_type_to_kelp_type(t_val.clone(), context_store),
        (Some(t_val), Some(t_sym)) => {
            if val_type_to_kelp_type(t_val.clone(), context_store) != t_sym {
                error!(
                    span,
                    "Cannot define the type of a symbol twice infered: {} and newly declared: {}",
                    t_sym,
                    val_type_to_kelp_type(t_val.clone(), context_store)
                );
            }
            t_sym
        }
    }
}

// This should return type of the type specified as <someSymbol>: <type> and return KelpType::Type or KelpType::Group/Lambda/Enum
fn val_type_to_kelp_type(val: KelpVal, context_store: &mut ContextStore) -> KelpType {
    let span = val.span();
    match val.inner() {
        _KelpVal::Sym { name, typ: _ } => match context_store.get_symbol_type(name) {
            //Some(t) => KelpType::Type { name: name.clone(), value: Rc::new(t)},
            Some(t) => t,
            None => {
                error!(span, "{} is not a defined symbol", name);
                KelpType::Unknown
            }
        },
        // TODO: Check if you can really do this
        _KelpVal::Lambda { arg, body, ret } => {
            typecheck_lambda(arg.clone(), body.clone(), ret.clone(), context_store).1
        }
        _KelpVal::Group(members) => {
            let m_types: Vec<_> = members
                .iter()
                .map(|m| val_type_to_kelp_type(m.clone(), context_store))
                .collect();

            KelpType::Group { members: m_types }
        }
        _KelpVal::Enum(_) => todo!("Implement enum as possible type for <symbol>: <type>"),
        _ => {
            error!(span, "{} cannot be used as a type", val);
            KelpType::Unknown
        }
    }
}

pub type AST = Node;

impl AST {
    pub fn construct(root_val: KelpVal) -> Self {
        let mut context_store = ContextStore::new();

        Node::from_value(root_val, &mut context_store)
    }
}

impl Display for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn format_seq(seq: impl IntoIterator<Item = impl Display>) -> String {
            seq.into_iter()
                .map(|m| format!("\t{}", m))
                .reduce(|acc, next| format!("{}\n{}", acc, next))
                .unwrap_or_else(|| "".to_string())
        }

        fn format_enum(enum_members: &IndexMap<String, (usize, Option<Node>)>) -> Vec<String> {
            enum_members
                .keys()
                .zip(enum_members.values())
                .map(|(key, value)| {
                    let node = if let Some(node) = value.1.clone() {
                        format!("{}", node)
                    } else {
                        "".to_string()
                    };
                    format!("\t{}: {} = {}", key, value.0, node)
                })
                .collect()
        }

        match &*self.inner.borrow() {
            _Node::Int64(i) => write!(f, "Int: '{}' Type: '{}'", i, self.typ),
            _Node::Float32(n) => write!(f, "Float: '{}' Type: '{}'", n, self.typ),
            _Node::Bool(b) => write!(f, "Bool: '{}' Type: '{}'", b, self.typ),
            _Node::String(s) => write!(f, "String: '{}' Type: '{}'", s, self.typ),
            _Node::Symbol(sym) => write!(f, "Symbol: '{}' Type: '{}'", sym, self.typ),
            _Node::Expression { op, lhs, rhs } => write!(
                f,
                "Expression:\n\tlhs: '{}'\n\top: '{}'\n\trhs: '{}'",
                lhs, op, rhs
            ),
            _Node::Lambda { arg, ret, body } => write!(
                f,
                "Lambda:\n\targ: '{}'\n\tret: '{}'\n\tbody: '{}'",
                arg, ret, body
            ),
            _Node::Body(exprs) => write!(f, "Body:\n{}\nType: {}", format_seq(exprs), self.typ),
            _Node::Group(members) => {
                write!(f, "Group:\n'{}'\nType: '{}'", format_seq(members), self.typ)
            }
            _Node::Enum(members) => write!(
                f,
                "Enum:\n'{}'\nType: '{}'",
                format_seq(format_enum(members)),
                self.typ
            ),
            _Node::Empty => write!(f, "Empty Type: '{}'", self.typ),
        }
    }
}
