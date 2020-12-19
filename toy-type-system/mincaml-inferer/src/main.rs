mod expr;
mod inference;
mod types;
fn main() {
    let alloc = Default::default();
    // let f = (fn x -> x) in f 3
    let x = expr::Expr::Variable("x".to_string());
    let f = expr::Expr::Variable("f".to_string());
    let fn_expr = expr::Expr::Lambda("x".to_string(), &x);
    let apply_expr = expr::Expr::Application(&f, &expr::Expr::Integer(3));
    let let_expr = expr::Expr::Let("f".to_string(), &fn_expr, &apply_expr);
    let t = inference::main(&alloc, let_expr);

    eprintln!("result => {:?}", t.unwrap());
}
