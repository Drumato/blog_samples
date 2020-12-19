#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type<'a> {
    Boolean,
    Integer,
    Fn(TypeRef<'a>, TypeRef<'a>),
    Variable(String),
}

pub type TypeRef<'a> = &'a Type<'a>;
