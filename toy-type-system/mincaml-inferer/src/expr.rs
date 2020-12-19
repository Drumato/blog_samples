#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Expr<'a> {
    /// `x`
    Variable(String),
    /// `42`
    Integer(i64),
    /// `true` | `false`
    Boolean(bool),
    /// `x + 3`
    Plus(ExprRef<'a>, ExprRef<'a>),
    /// `\x -> x + 3`
    Lambda(String, ExprRef<'a>),
    /// `f 3`
    Application(ExprRef<'a>, ExprRef<'a>),
    /// `let x = 3 in x + 3`
    Let(String, ExprRef<'a>, ExprRef<'a>),
}

pub type ExprRef<'a> = &'a Expr<'a>;
