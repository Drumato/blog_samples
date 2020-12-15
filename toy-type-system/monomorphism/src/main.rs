mod expr;
mod infer;
mod types;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let t = infer::analyze(expr::Expr::Let(
        "x",
        &expr::Expr::Integer(42),
        &expr::Expr::BinOp(
            expr::Operator::Plus,
            &expr::Expr::Variable("x"),
            &expr::Expr::Integer(3),
        ),
    ))?;
    eprintln!("{:?}", t);

    Ok(())
}
