use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Boolean,
    Integer,
    Fn(Box<Type>, Box<Type>),
    Variable(String),
}

#[derive(Debug, Clone)]
pub struct Env {
    pub type_vars: HashMap<String, Type>,
    pub vars_in_fn: HashMap<String, Type>,
}

impl Default for Env {
    fn default() -> Self {
        Self {
            type_vars: Default::default(),
            vars_in_fn: Default::default(),
        }
    }
}
