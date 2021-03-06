use crate::{Error, ast::Expr, message::{ErrorType, MessageDispatcher}};
use std::collections::HashMap;
use std::collections::HashSet;
use uuid::Uuid;


use self::builder::OperatorBuilder;
use self::builder::OperatorDef;

mod builder;

macro_rules! corrupt_layer {
    ($e:expr,$s:ident) => {{
        match $e {
            Ok(layer) => layer,
            Err(e) => {
                eprintln!("{}", e);
                $s.had_error = true;
            }
        }
    }};
}

#[derive(Debug, Clone)]
pub enum Associativity {
    Left,
    Right,
    Neutral,
}

pub type Operator = String;

#[derive(Default, Debug, Clone)]
pub struct OperatorList {
    operators: HashMap<Operator, (Uuid, Associativity)>,
    layers: HashMap<Uuid, OperatorLayer>,
    had_error: bool,
}

#[derive(Debug, Clone)]
pub struct OperatorLayer {
    pub parents: HashSet<Uuid>,
    pub children: HashSet<Uuid>,
}

impl OperatorList {
    // fn get_start_nodes(&self) -> Vec<Uuid> {
    //     self.layers
    //         .iter()
    //         .filter(|(uuid, layer)| layer.parents.len() == 0)
    //         .map(|(uuid, _)| uuid.clone())
    //         .collect()
    //}
    // pub fn sort_list(&mut self) -> Result<(), Error> {
    //     let mut sorted_layers: HashMap<Uuid, OperatorLayer> = HashMap::default();
    //     let unsorted_layers = self.layers.clone();
    //     let starter_nodes = self.get_start_nodes();

    //     for node in starter_nodes {
    //         let layer = unsorted_layers.get(&node).unwrap().clone();
    //         sorted_layers.insert(node, layer);

    //         unsorted_layers.filter()
    //     }

    //     Ok(())
    // }

    pub fn is_above(&self, uuid_to_find: &Uuid, own_uuid: &Uuid) -> bool {
        let own_layer = self.layers.get(own_uuid).unwrap();
        if own_layer.parents.contains(uuid_to_find) {
            true
        } else if own_layer.parents.len() == 0 {
            false
        } else {
            own_layer
                .parents
                .iter()
                .filter(|parent_uuid| self.is_above(uuid_to_find, parent_uuid) == true)
                .count()
                > 0
        }
    }

    fn update_children(&mut self, child_uuid: &Uuid, own_uuid: &Uuid) -> Result<(), Error> {
        Ok(if let Some(own_layer) = self.layers.get_mut(own_uuid) {
            if !own_layer.children.contains(child_uuid) {
                own_layer.children.insert(child_uuid.clone());
            }
        } else {
            return Err(Error::default()
                .with_message("target layer does not exist".to_string())
                .with_type(ErrorType::OperatorDefinitionError)
                .build());
        })
    }
    fn update_parents(&mut self, parent_uuid: &Uuid, own_uuid: &Uuid) -> Result<(), Error> {
        Ok(if let Some(own_layer) = self.layers.get_mut(own_uuid) {
            if !own_layer.parents.contains(parent_uuid) {
                own_layer.parents.insert(parent_uuid.clone());
            }
        } else {
            return Err(Error::default()
                .with_message("target layer does not exist".to_string())
                .with_type(ErrorType::OperatorDefinitionError)
                .build());
        })
    }

    pub fn with_layer(
        &mut self,
        layer: OperatorLayer,
        operator: Operator,
        assoc: Associativity,
    ) -> Result<&mut Self, Error> {
        let uuid = Uuid::new_v4();

        // these two update the graph, so every layer has both up to date references to other
        let _ = layer
            .parents
            .iter()
            .map(|parent_uuid| corrupt_layer!(self.update_children(&uuid, parent_uuid), self));
        let _ = layer
            .children
            .iter()
            .map(|child_uuid| corrupt_layer!(self.update_parents(&uuid, child_uuid), self));
        self.layers.insert(uuid, layer);

        corrupt_layer!(self.add_operator(operator, uuid, assoc), self);
        Ok(self)
    }

    fn add_operator(
        &mut self,
        operator: Operator,
        uuid: Uuid,
        assoc: Associativity,
    ) -> Result<(), Error> {
        if self.operators.contains_key(&operator) {
            return Err(Error::default()
                .with_type(ErrorType::OperatorDefinitionError)
                .with_message(format!("operator {} is already defined", operator))
                .build());
        } else {
            self.operators.insert(operator, (uuid, assoc));
        }
        Ok(())
    }

    pub fn with_operator(
        &mut self,
        operator_target: Operator,
        operator_projectile: Operator,
        projectile_assoc: Associativity,
    ) -> Result<&mut Self, Error> {
        match self.operators.get(&operator_target) {
            Some((uuid, _)) => {
                let uuid = uuid.clone();
                corrupt_layer!(
                    self.add_operator(operator_projectile, uuid, projectile_assoc),
                    self
                );
            }
            None => {
                return Err(Error::default()
                    .with_type(ErrorType::OperatorDefinitionError)
                    .with_message(format!(
                        "operator {} does not exist in any layer",
                        operator_target
                    ))
                    .build())
            }
        };
        Ok(self)
    }

    pub fn get_uuid(&self, operator: &Operator) -> Result<Uuid, Error> {
        match self.operators.get(operator) {
            Some(tuple) => Ok(tuple.0),
            None => Err(Error::default()
                .with_message(format!("uuid for {} does not exist", operator))
                .build()),
        }
    }

    pub fn build(&mut self) -> Self {
        let list = std::mem::take(self);
        list
    }
}

fn build_layers(
    list: &mut OperatorList,
    targets_up: &Vec<Operator>,
    targets_down: &Vec<Operator>,
    assoc: &Associativity,
    operator: &Operator,
) -> Result<(), Error> {
    list.with_layer(
        OperatorLayer {
            parents: targets_up
                .iter()
                .map(|op| match list.get_uuid(op) {
                    Ok(uuid) => uuid,
                    Err(e) => {
                        eprintln!("{}", e);
                        Uuid::from_u128(0)
                    }
                }) // does not currently check for duplicates. Probably will panic oops
                .collect(),
            children: targets_down
                .iter()
                .map(|op| match list.get_uuid(op) {
                    Ok(uuid) => uuid,
                    Err(e) => {
                        eprintln!("{}", e);
                        Uuid::from_u128(0)
                    }
                })
                .collect(),
        },
        operator.to_string(),
        assoc.clone(),
    )?;
    Ok(())
}
fn build_operator(
    list: &mut OperatorList,
    target: &Operator,
    operator: &Operator,
    assoc: &Associativity,
) -> Result<(), Error> {
    list.with_operator(target.clone(), operator.to_string(), assoc.clone())?;
    Ok(())
}

pub fn build_operators(ast: Expr, msg_dispatcher: MessageDispatcher) -> Result<OperatorList, Error> {
    let defs = OperatorBuilder::build(ast, msg_dispatcher)?;
    let mut list = OperatorList::default();
    let _ = defs.iter().map(|def| match def {
        OperatorDef::Operator {
            ref targets_up,
            ref targets_down,
            ref assoc,
            ref operator,
        } => match build_layers(&mut list, targets_up, targets_down, assoc, operator) {
            Ok(()) => (),
            Err(e) => eprintln!("{}", e),
        },
        OperatorDef::Layer {
            ref target,
            ref assoc,
            ref operator,
        } => match build_operator(&mut list, target, operator, assoc) {
            Ok(()) => (),
            Err(e) => eprintln!("{}", e),
        },
    });
    list.build();
    Ok(list)
}
