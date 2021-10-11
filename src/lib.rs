#![feature(backtrace)]

use std::collections::HashMap;

use node::Node;
use token::Token;
use typ::{BitWidth, Signed, Type};

pub use crate::message::{info, warning};
use crate::{
    node::{get_span, NodeInner},
    typ::ErrorType,
};

#[macro_use]
pub mod message;

mod node;
mod reader;
mod span;
mod token;
mod typ;
pub mod link;

#[derive(Debug, Clone)]
struct SymbolProperties {
    typ: Type,
    is_operator: bool,
}

#[derive(Debug, Clone)]
struct Context {
    macro_map: HashMap<String, Node>,
    macro_symbol_map: HashMap<String, Node>,
    type_map: HashMap<String, Type>,
    symbol_map: HashMap<String, SymbolProperties>,
    parent_context_id: Option<String>,
    children_context_ids: Vec<String>,
}

impl Context {
    fn new(parent_context_id: Option<String>) -> Self {
        Self {
            macro_map: HashMap::new(),
            macro_symbol_map: HashMap::new(),
            type_map: HashMap::new(),
            symbol_map: HashMap::new(),
            parent_context_id,
            children_context_ids: Vec::new(),
        }
    }
}

pub struct Compiler {
    context_store: HashMap<String, Context>,
    current_context_id: String,
    current_id: usize,
}

impl Compiler {
    fn get_parent_context(&self, context_id: &String) -> Option<String> {
        self.context_store
            .get(context_id)
            .expect("Context doesn't exist")
            .parent_context_id
            .clone()
    }
    fn get_symbol_type_from_context(
        &self,
        symbol: &String,
        context_id: &String,
    ) -> Option<(SymbolProperties, String)> {
        if let Some(props) = self.context_store[context_id].symbol_map.get(symbol) {
            Some((props.clone(), context_id.clone()))
        } else if let Some(ref parent_context_id) = self.get_parent_context(context_id) {
            self.get_symbol_type_from_context(symbol, parent_context_id)
        } else {
            //TODO: Included contexts
            None
        }
    }
    fn get_symbol_type(&self, symbol: &String) -> Option<(SymbolProperties, String)> {
        self.get_symbol_type_from_context(symbol, &self.current_context_id)
    }
    fn set_types_type(&mut self, symbol: String, typ: Type) -> Option<Type> {
        self.context_store
            .get_mut(&self.current_context_id)
            .unwrap()
            .type_map
            .insert(symbol, typ)
    }
    fn get_types_type_from_context(
        &self,
        symbol: &String,
        context_id: &String,
    ) -> Option<(Type, String)> {
        if let Some(types_type) = self.context_store[context_id].type_map.get(symbol) {
            Some((types_type.clone(), context_id.clone()))
        } else if let Some(ref parent_context_id) = self.get_parent_context(context_id) {
            self.get_types_type_from_context(symbol, parent_context_id)
        } else {
            //TODO: Included contexts
            None
        }
    }
    fn get_types_type(&self, symbol: &String) -> Option<(Type, String)> {
        self.get_types_type_from_context(symbol, &self.current_context_id)
    }
    fn set_symbol_in_context(
        &mut self,
        symbol: &String,
        props: SymbolProperties,
        context_id: &String,
    ) {
        self.context_store
            .get_mut(context_id)
            .unwrap()
            .symbol_map
            .insert(symbol.clone(), props);
    }
    fn set_symbol(&mut self, key: Node, value: Node, is_operator: bool) -> Node {
        match key.inner() {
            NodeInner::Symbol(ref s) => {
                // let docstring = match docstring {
                //     Some(d) => match d.inner() {
                //         NodeInner::String(ref s) => s.clone(),
                //         _ => "".to_string()
                //     },
                //     _ => "".to_string()
                // };
                // self.context_store.get_mut(&self.current_context_id).unwrap()
                //     .docstring_map
                //     .insert(s.clone(), docstring);

                match self.get_symbol_type(s) {
                    // This matches if the symbol already exists and checks for type
                    Some((ref props, _)) if &props.typ == value.typ() => {
                        self.set_symbol_in_context(
                            s,
                            SymbolProperties {
                                typ: value.typ().clone(),
                                is_operator,
                            },
                            &self.current_context_id.clone(),
                        );
                        key.with_inner(NodeInner::SetCall {
                            symbol: s.clone(),
                            value: value.clone(),
                        })
                        .with_type(value.typ().clone())
                    }
                    // This matches if the types don't match, and throws an error, but compiles
                    Some((_, _)) => {
                        let span = key.span();
                        error!(span, "Symbols cannot change type during runtime");
                        key.with_inner(NodeInner::SetCall {
                            symbol: s.clone(),
                            value: value.clone(),
                        })
                        .with_type(value.typ().clone())
                    }
                    // This matches if the symbol doesn't exist. It produces  instead of `SetCall`
                    None => key
                        .with_inner(NodeInner::DefSymCall {
                            symbol: s.clone(),
                            value: value.clone(),
                        })
                        .with_type(value.typ().clone()),
                }
            }
            _ => {
                let span = key.span();
                error!(span, "Cannot define as symbol as key is not symbol");
                Node::unknown(span).with_type(Type::Error(ErrorType::Definition))
            }
        }
    }
    fn get_macro_definition(&self, symbol: &String) -> Option<Node> {
        self.context_store[&self.current_context_id]
            .macro_map
            .get(symbol)
            .cloned()
    }
    fn set_macro_definition(&mut self, key: Node, value: Node) {
        match (key.inner(), value.inner()) {
            (NodeInner::Symbol(ref key_sym), NodeInner::MacroDef { .. }) => {
                match self.get_macro_definition(key_sym) {
                    Some(_) => {
                        let span = key.span();
                        error!(span, "Cannot define two same named macros!");
                    }
                    None => {self
                        .context_store
                        .get_mut(&self.current_context_id)
                        .unwrap()
                        .macro_map
                        .insert(key_sym.clone(), value);
                        }
                }
            }
            (NodeInner::Symbol(_), _) => {
                let span = value.span();
                error!(span, "Cannot define value as macro definition");
            }
            (_, _) => {
                let span = key.span();
                error!(span, "Cannot define macro as key is not a symbol");
            }
        }
    }
    fn get_macro_symbol_node_in_context(
        &self,
        symbol: &String,
        context_id: &String,
    ) -> Option<Node> {
        if let Some(node) = self.context_store[context_id].macro_symbol_map.get(symbol) {
            Some(node.clone())
        } else if let Some(ref parent_context_id) = self.get_parent_context(context_id) {
            self.get_macro_symbol_node_in_context(symbol, parent_context_id)
        } else {
            None
        }
    }
    fn get_macro_symbol_node(&self, symbol: String) -> Option<Node> {
        self.get_macro_symbol_node_in_context(&symbol, &self.current_context_id)
    }
    fn set_macro_symbol(&mut self, symbol: String, value: Node) -> Option<Node> {
        self.context_store
            .get_mut(&self.current_context_id)
            .unwrap()
            .macro_symbol_map
            .insert(symbol, value)
    }
    fn add_child_context(&mut self) -> String {
        self.current_id += 1;
        let id = format!("{}/{}", self.current_context_id, self.current_id);

        self.context_store
            .get_mut(&self.current_context_id)
            .unwrap()
            .children_context_ids
            .push(id.clone());
        self.context_store.insert(
            id.clone(),
            Context::new(Some(self.current_context_id.clone())),
        );

        self.current_context_id = id.clone();
        id
    }
    fn escape_context(&mut self) -> Option<String> {
        match self.context_store[&self.current_context_id.clone()].parent_context_id {
            Some(ref id) => {
                self.current_context_id = id.clone();
                Some(id.clone())
            }
            None => None,
        }
    }

    pub fn new() -> Self {
        Self {
            context_store: HashMap::new(),
            current_context_id: "".to_string(),
            current_id: 0,
        }
    }

    fn is_macro_call(&self, node: &Node) -> Option<(Node, Vec<String>, Vec<Node>)> {
        match node.inner() {
            NodeInner::Sequence(ref sequence) => match sequence[0].inner() {
                NodeInner::Symbol(ref sym) => match self.get_macro_definition(sym) {
                    Some(macro_def) => match macro_def.inner() {
                        NodeInner::MacroDef { params, seq } => {
                            Some((seq.clone(), params.clone(), sequence[1..].to_vec()))
                        }
                        _ => None,
                    },
                    _ => None,
                },
                _ => None,
            },
            _ => None,
        }
    }

    fn apply_macro(&mut self, node: Node, params: Vec<(String, Node)>) -> Node {
        for (key, value) in params {
            self.set_macro_symbol(key.clone(), value.clone());
        }

        self.eval(node)
    }

    fn macroexpand(&mut self, mut node: Node) -> (bool, Node) {
        let mut was_expanded = false;
        while let Some((seq, param_keys, params)) = self.is_macro_call(&node) {
            let param_kvs = param_keys.iter().cloned().zip(params).collect();
            node = self.apply_macro(seq, param_kvs);
            was_expanded = true;
        }
        (was_expanded, node)
    }

    fn quasiquote(&self, node: Node) -> Node {
        match node.inner() {
            NodeInner::Sequence(ns) if ns.len() > 0 => {
                todo!()
                //let a0
            }
            _ => node.quote(),
        }
    }

    ///TODO: Check if params are typed and correct, construct Vec of keyval pairs
    ///TODO: Also register them in the context
    fn params(&mut self, node: Node) -> Vec<Node> {
        match node.inner() {
            NodeInner::List(ns) => {
                let ns = ns
                    .chunks(2)
                    .map(|c| {
                        let symbol = c[0].clone();
                        let typ = c[1].clone();

                        match typ.inner() {
                            NodeInner::Symbol(typ_sym) => {
                                if let Some(typ_type) = self.get_types_type(typ_sym) {
                                    let symbol = symbol.with_type(typ_type.0);
                                    self.set_symbol(
                                        symbol.clone(),
                                        symbol.with_inner(NodeInner::Empty),
                                        false,
                                    );

                                    symbol
                                } else {
                                    let span = typ.span();
                                    error!(span, "Type does not exist, did you forget the ':'?");
                                    symbol
                                }
                            }
                            _ => {
                                let span = typ.span();
                                error!(span, "Parameters type must be a symbol");
                                symbol
                            }
                        }
                    })
                    .collect();

                ns
            }
            NodeInner::UncheckedTuple(_tups) => {
                todo!()
            }
            _ => {
                let span = node.span();
                error!(span, "Params are not of a compatible type");
                vec![]
            }
        }
    }

    fn eval(&mut self, mut node: Node) -> Node {
        let ret: Node;
        //println!("{}", node);
        'tco: loop {
            //println!("TCO: {:#?}", node.inner());
            ret = match node.clone().inner() {
                NodeInner::Quote(n) => n.clone(),
                NodeInner::Quasiquote(n) => {
                    self.quasiquote(n.clone());
                    continue 'tco;
                }
                NodeInner::Eval(n) => self.eval(n.clone()),
                NodeInner::Body(seqs) => {
                    node.with_inner(NodeInner::Body(seqs.iter().map(|s| self.eval(s.clone())).collect()))
                }
                NodeInner::Sequence(ns) => {
                    if ns.len() == 0 {
                        return node.clone();
                    }

                    // Expand macros
                    match self.macroexpand(node.clone()) {
                        (true, new_node) => {
                            node = new_node;
                            continue 'tco;
                        }
                        _ => (),
                   }

                    let a0 = &ns[0];

                    match a0.inner() {
                        NodeInner::Symbol(ref a0sym) if a0sym == "def" => {
                            let value = self.eval(ns[2].clone());
                            self.set_symbol(ns[1].clone(), value, false)
                        }
                        NodeInner::Symbol(ref a0sym) if a0sym == "defmacro" => {
                            //NOTE: (Probably doesn't need to be done) Add some kind of name+param to avoid conflicts with other macros
                            let (a1, a2, a3) = (ns[1].clone(), ns[2].clone(), ns[3].clone());

                            let params: Vec<String> = match a2.inner() {
                                NodeInner::List(l) => l
                                    .iter()
                                    .filter_map(|ll| match ll.inner() {
                                        NodeInner::Symbol(ref ll_sym) => Some(ll_sym.clone()),
                                        _ => {
                                            let span = ll.span();
                                            error!(span, "Parameter is not a symbol!");
                                            None
                                        }
                                    })
                                    .collect(),
                                _ => {
                                    let span = a2.span();
                                    error!(span, "Macro parameters are not a list!");
                                    vec![]
                                }
                            };

                            let macro_def = a3
                                .clone()
                                .with_inner(NodeInner::MacroDef { params, seq: a3 });

                            self.set_macro_definition(a1, macro_def.clone());
                            macro_def.with_inner(NodeInner::Empty).with_type(Type::Empty)
                        }
                        NodeInner::Symbol(ref a0sym) if a0sym == "defop" => {
                            let (op, value) = (ns[1].clone(), ns[2].clone());
                            let value = self.eval(value);
                            self.set_symbol(op, value, true)
                        }
                        NodeInner::Symbol(ref a0sym) if a0sym == "fn*" => {
                            let (params, ret, seq) = (ns[1].clone(), ns[2].clone(), ns[3].clone());

                            let ret_typ = match ret.inner() {
                                NodeInner::Symbol(ref s) => match self.get_types_type(s) {
                                    Some((typ, _)) => typ,
                                    None => {
                                        let span = ret.span();
                                        error!(
                                            span,
                                            "Type is not known, did you forget the ':' prefix?"
                                        );
                                        Type::Error(ErrorType::InvalidType)
                                    }
                                },
                                _ => {
                                    let span = ret.span();
                                    error!(span, "Return must be a symbol of a valid type");
                                    Type::Error(ErrorType::InvalidType)
                                }
                            };
                            let params = self.params(params);
                            //println!("params: {:?}", params);

                            self.add_child_context();
                            let lambda = a0
                                .with_inner(NodeInner::Lambda {
                                    params: params.clone(),
                                    seq: self.build(seq),
                                    context_id: self.current_context_id.clone(),
                                })
                                .with_type(Type::Function {
                                    params: params.iter().map(|p| p.typ().clone()).collect(),
                                    ret: Box::new(ret_typ),
                                });
                            self.escape_context();
                            lambda
                        }
                        NodeInner::Symbol(ref a0sym) if a0sym == "quote" => {
                            node = ns[1].quote();
                            continue 'tco;
                        }
                        NodeInner::Symbol(ref a0sym) if a0sym == "quasiquote" => {
                            node = ns[1].quasiquote();
                            continue 'tco;
                        }
                        NodeInner::Symbol(ref a0sym) if a0sym == "deftype!" => {
                            todo!()
                        }
                        NodeInner::Symbol(ref a0sym) if a0sym == "use" => {
                            for n in &ns[1..] {
                                match n.inner() {
                                    NodeInner::Symbol(ref str) | NodeInner::String(ref str) => {

                                    },
                                    _ => {
                                        let span = n.span();
                                        error!(span, "Use can be only used with symbol or a string")
                                    }
                                }
                            }

                            todo!()
                        }
                        _ => {
                            let built_node = self.build(node);
                            match built_node.inner() {
                                NodeInner::Sequence(ns) => {
                                    //let ref fun = ns[0];
                                    //TODO: There should be all the weird and complex logic to correctly evaluate sequences

                                    let mut fn_vec: Vec<_> = vec![];
                                    let mut op_vec: Vec<_> = vec![];

                                    for (i, n) in ns.iter().enumerate() {
                                        match n.inner() {
                                            NodeInner::DefSymCall { symbol, value } => {
                                                if let Type::Function { .. } = value.typ() {
                                                    if self
                                                        .get_symbol_type(symbol)
                                                        .unwrap()
                                                        .0
                                                        .is_operator
                                                    {
                                                        op_vec.push((i, symbol));
                                                    } else {
                                                        fn_vec.push((i, symbol));
                                                    }
                                                }
                                            }
                                            NodeInner::GetCall { symbol } => {
                                                if self
                                                    .get_symbol_type(symbol)
                                                    .unwrap()
                                                    .0
                                                    .is_operator
                                                {
                                                    op_vec.push((i, symbol));
                                                } else {
                                                    fn_vec.push((i, symbol));
                                                }
                                            }
                                            _ => (),
                                        }
                                    }

                                    let op_index =
                                        ns.iter().enumerate().find_map(|(index, node)| match node
                                            .inner()
                                        {
                                            NodeInner::DefSymCall { symbol, .. } => {
                                                if self
                                                    .get_symbol_type(symbol)
                                                    .unwrap()
                                                    .0
                                                    .is_operator
                                                {
                                                    Some((index, symbol))
                                                } else {
                                                    None
                                                }
                                            }
                                            NodeInner::GetCall { symbol } => {
                                                if self
                                                    .get_symbol_type(symbol)
                                                    .unwrap()
                                                    .0
                                                    .is_operator
                                                {
                                                    Some((index, symbol))
                                                } else {
                                                    None
                                                }
                                            }
                                            _ => None,
                                        });

                                    if let Some((index, op)) = op_index {
                                        let lhs_vec = ns[0..index].to_vec();
                                        let rhs_vec = ns[index..ns.len() - 1].to_vec();

                                        let lhs_node = self.eval(
                                            ns[0]
                                                .with_inner(NodeInner::Sequence(lhs_vec.clone()))
                                                .with_span(get_span(&lhs_vec)),
                                        );
                                        let rhs_node = self.eval(
                                            ns[0]
                                                .with_inner(NodeInner::Sequence(lhs_vec.clone()))
                                                .with_span(get_span(&rhs_vec)),
                                        );

                                        let op_typ = self.get_symbol_type(op).unwrap().0.typ;

                                        match op_typ {
                                            Type::Function { params, ret } => {
                                                if params
                                                    == vec![
                                                        lhs_node.typ().clone(),
                                                        rhs_node.typ().clone(),
                                                    ]
                                                {
                                                    built_node
                                                        .with_inner(NodeInner::FunCall {
                                                            function: op.clone(),
                                                            params: vec![lhs_node, rhs_node],
                                                        })
                                                        .with_type(*ret)
                                                } else {
                                                    let span = get_span(&vec![lhs_node, rhs_node]);
                                                    error!(
                                                        span,
                                                        "Expected and provided types do not match"
                                                    );
                                                    Node::unknown(span)
                                                }
                                            }
                                            _ => {
                                                let span = ns[index].span();
                                                error!(span, "Symbol is not a operator function");
                                                Node::unknown(span)
                                            }
                                        }
                                    } else {
                                        // TODO: Allow for simple sequences
                                        let mut nodes: Vec<Node> = vec![];
                                        'seq: for n in ns {
                                        match n.inner() {
                                            NodeInner::Symbol(a0sym) => match self.get_symbol_type(a0sym) {
                                                Some((props, _context_id)) => match props.typ {
                                                    Type::Function { params, ret } => {
                                                        if params.len() > (ns.len() - 2) {
                                                            if let Some((_, tail)) =
                                                                ns.split_first()
                                                            {
                                                                for (i, (t, p)) in tail
                                                                    .iter()
                                                                    .zip(params.clone())
                                                                    .enumerate()
                                                                {
                                                                    if i == ns.len() - 2 {
                                                                        break;
                                                                    }

                                                                    if t.typ() != &p {
                                                                        let span = t.span();
                                                                        error!(span, "Parameter types do not match");
                                                                    }
                                                                }

                                                                nodes.push (built_node
                                                                    .with_inner(
                                                                        NodeInner::PartialFunCall {
                                                                            function: a0sym.clone(),
                                                                            params: tail.to_vec(),
                                                                            missing_params: params
                                                                                .len()
                                                                                - (ns.len() - 2),
                                                                        },
                                                                    )
                                                                    .with_type(Type::Function {
                                                                        params: params
                                                                            [(ns.len() - 2)..]
                                                                            .to_vec(),
                                                                        ret,
                                                                    }));
                                                                break 'seq;
                                                            } else {
                                                                nodes.push(built_node.with_inner(
                                                                    NodeInner::PartialFunCall {
                                                                        function: a0sym.clone(),
                                                                        params: ns[1..].to_vec(),
                                                                        missing_params: params
                                                                            .len(),
                                                                    },
                                                                ));
                                                                break 'seq;
                                                            }
                                                        } else {
                                                            // FunCall if params match and has some spare
                                                            if let Some((_, tail)) =
                                                                ns.split_first()
                                                            {
                                                                for (t, p) in
                                                                    tail.iter().zip(params)
                                                                {
                                                                    if t.typ() != &p {
                                                                        let span = t.span();
                                                                        error!(span, "Parameter types do not match");
                                                                    }
                                                                }
                                                                nodes.push(built_node
                                                                    .with_inner(
                                                                        NodeInner::FunCall {
                                                                            function: a0sym.clone(),
                                                                            params: tail.to_vec(),
                                                                        },
                                                                    )
                                                                    .with_type(*ret));
                                                                break 'seq;
                                                            } else {
                                                                nodes.push(built_node
                                                                    .with_inner(
                                                                        NodeInner::FunCall {
                                                                            function: a0sym.clone(),
                                                                            params: vec![],
                                                                        },
                                                                    )
                                                                    .with_type(*ret));
                                                                break 'seq;
                                                            }
                                                        }
                                                    }
                                                    _ => {
                                                        nodes.push(n.with_inner(NodeInner::GetCall{
                                                            symbol: a0sym.clone(),
                                                        }));
                                                    }
                                                }
                                                None => {
                                                    let span = n.span();
                                                    error!(span, "Symbol does not exist");
                                                }
                                            }
                                            _ => {
                                                nodes.push(n.clone())
                                            }
                                        }}

                                        built_node.with_inner(NodeInner::Sequence(nodes))
                                    }


                                    // fn funcall_op(op_vec: Vec<(usize, String)>, fn_vec: Vec<(usize, String)>, ns: Vec<Node>) -> Node {
                                    //     let (lhs, rhs) = (ns.get(ns.len() - 3), ns.get( ns.len() - 1));

                                    //     if lhs.and(rhs).is_none() {
                                    //         let (lhs, rhs) = (lhs.unwrap(), rhs.unwrap());

                                    //     }
                                    // }

                                    // fn funcall_fun(fn_vec: Vec<(usize, String)>, ns: Vec<Node>) -> Node {

                                    // }

                                    // for (i, op) in op_vec.iter().enumerate().rev() {
                                    //     let lhs = ns[..op.0 - 1].to_vec();
                                    //     let rhs = ns[op.0 + 1..].to_vec();
                                    //     let new_op_vec = op_vec.remove(index)
                                    // }
                                }
                                _ => {
                                    let span = built_node.span();
                                    error!(span, "Expected a sequence");

                                    Node::unknown(span)
                                        .with_type(Type::Error(ErrorType::NotSequence))
                                }
                            }
                        }
                    }
                }
                _ => {
                    println!("Building node {}", node);
                    self.build(node)
                },
            };
            break;
        }
        ret
    }

    fn build(&mut self, node: Node) -> Node {
        match node.inner() {
            NodeInner::Symbol(ref name) => {
                if let Some(node) = self.get_macro_symbol_node(name.clone()) {
                    node
                } else if let Some((props, _)) = self.get_symbol_type(name) {
                    node.with_inner(NodeInner::GetCall {
                        symbol: name.clone(),
                    })
                    .with_type(props.typ)
                } else {
                    let span = node.span();
                    error!(span, "Symbol '{}' not found", name);
                    node
                }
            }
            NodeInner::Sequence(ref l) => {
                //TODO: Check for arrays lol
                let nodes = l.iter().map(|n| self.eval(n.clone())).collect();

                node.with_inner(NodeInner::Sequence(nodes))
            }
            _ => node.clone(),
        }
    }

    fn read(&self, str: &str, path: String) -> Token {
        reader::read_string(str.to_string(), path)
    }

    fn insert_default_types(&mut self) {
        let types = [
            (":i8", Type::Int(BitWidth::Eight, Signed::Signed)),
            (":i16", Type::Int(BitWidth::Sixteen, Signed::Signed)),
            (":i32", Type::Int(BitWidth::ThirtyTwo, Signed::Signed)),
            (":i64", Type::Int(BitWidth::SixtyFour, Signed::Signed)),
            (":u8", Type::Int(BitWidth::Eight, Signed::Unsigned)),
            (":u16", Type::Int(BitWidth::Sixteen, Signed::Unsigned)),
            (":u32", Type::Int(BitWidth::ThirtyTwo, Signed::Unsigned)),
            (":u64", Type::Int(BitWidth::SixtyFour, Signed::Unsigned)),
        ];

        for (typ_sym, typ_type) in types {
            self.set_types_type(typ_sym.to_string(), typ_type);
        }
    }

    pub fn rep(&mut self, str: &str, path: String) -> String {
        let token_tree = self.read(str, format!("file://{}", path));

        self.context_store
            .insert("".to_string(), Context::new(None));
        self.insert_default_types();

        //println!("unevaled: {}\n-----", Node::from(token_tree.clone()));

        let node_tree = self.build(token_tree.into());

        println!("evaled: {}\n-----", node_tree);

        String::new()
    }
}
