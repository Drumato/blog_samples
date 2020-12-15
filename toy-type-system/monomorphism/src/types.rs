use std::collections::HashMap;
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Integer,
    Boolean,
}

pub struct Env<'a> {
    pub env: HashMap<&'a str, Type>,
}

impl<'a> Default for Env<'a> {
    fn default() -> Self {
        Self {
            env: HashMap::new(),
        }
    }
}
