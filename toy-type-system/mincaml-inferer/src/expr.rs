#[allow(dead_code)]
#[derive(Debug, Clone)]
pub enum Expr {
    /// `x`
    Variable(String),
    /// `42`
    Integer(i64),
    /// `true` | `false`
    Boolean(bool),
    /// `x + 3`
    Plus(Box<Expr>, Box<Expr>),
    /// `\x -> x + 3`
    Lambda(String, Box<Expr>),
    /// `f 3`
    Application(Box<Expr>, Box<Expr>),
    /// `let x = 3 in x + 3`
    Let(String, Box<Expr>, Box<Expr>),
}
