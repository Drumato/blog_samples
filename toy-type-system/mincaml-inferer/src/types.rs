#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    Boolean,
    Integer,
    Fn(Box<Type>, Box<Type>),
    Variable(String),
}
