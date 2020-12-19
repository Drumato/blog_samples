mod expr;
mod inference;
mod types;
fn main() -> Result<(), Box<dyn std::error::Error>> {
    // let f = (fn x -> x) in f 3
    let x = expr::Expr::Variable("x".to_string());
    let f = expr::Expr::Variable("f".to_string());
    let fn_expr = expr::Expr::Lambda("x".to_string(), Box::new(x));
    let apply_expr = expr::Expr::Application(Box::new(f), Box::new(expr::Expr::Integer(3)));
    let let_expr = expr::Expr::Let("f".to_string(), Box::new(fn_expr), Box::new(apply_expr));
    let t = inference::main(let_expr)?;

    eprintln!("result => {:?}", t);

    Ok(())
}
