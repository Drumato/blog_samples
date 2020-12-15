#[derive(Debug, Copy, Clone)]
#[allow(dead_code)]
pub enum Expr<'a> {
    /// x
    Variable(&'a str),
    /// 42
    Integer(i64),
    /// true
    True,
    /// false
    False,
    /// e1 op e2
    BinOp(Operator, ExprRef<'a>, ExprRef<'a>),
    /// if e1 then e2 else e3
    IfElse(ExprRef<'a>, ExprRef<'a>, ExprRef<'a>),
    /// let x = e1 in e2
    Let(&'a str, ExprRef<'a>, ExprRef<'a>),
}

pub type ExprRef<'a> = &'a Expr<'a>;

#[allow(dead_code)]
#[derive(Debug, Copy, Clone)]
pub enum Operator {
    /// +
    Plus,
    /// *
    Multiply,
}
