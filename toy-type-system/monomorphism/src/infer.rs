use crate::expr::{Expr, Operator};
use crate::types::{Env, Type};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum InferenceError<'a> {
    #[error("cannot inference from {e:?}")]
    CannotInference { e: Expr<'a> },
    #[error("if and else have incompatible types")]
    IfAndElseHaveIncompatibleTypes,
    #[error("`{op:?}` must have two integer types")]
    BinaryOperatorMustHaveTwoIntegers { op: Operator },
    #[error("condition in if-else expression must be a boolean")]
    ConditionMustBeABoolean,
}

pub fn analyze<'a>(e: Expr<'a>) -> Result<Type, InferenceError<'a>> {
    let env = Default::default();
    let (t, _) = infer(e, env)?;
    Ok(t)
}

fn infer<'a>(e: Expr<'a>, env: Env<'a>) -> Result<(Type, Env<'a>), InferenceError<'a>> {
    match e {
        Expr::Integer(_v) => Ok((Type::Integer, env)),
        Expr::True | Expr::False => Ok((Type::Boolean, env)),
        Expr::IfElse(cond, body, alter) => {
            let (cond_t, env) = infer(*cond, env)?;
            if cond_t != Type::Boolean {
                return Err(InferenceError::ConditionMustBeABoolean);
            }
            let (body_t, env) = infer(*body, env)?;
            let (alter_t, env) = infer(*alter, env)?;
            if body_t != alter_t {
                return Err(InferenceError::IfAndElseHaveIncompatibleTypes);
            }

            Ok((body_t, env))
        }
        Expr::Let(name, e1, e2) => {
            let (t1, mut env) = infer(*e1, env)?;

            // 新しく型環境に登録
            env.env.insert(name, t1);

            infer(*e2, env)
        }
        Expr::Variable(name) => match env.env.get(name) {
            Some(t) => Ok((*t, env)),
            None => Err(InferenceError::CannotInference { e }),
        },
        Expr::BinOp(op, lhs, rhs) => {
            let (t1, env) = infer(*lhs, env)?;
            let (t2, env) = infer(*rhs, env)?;

            if t1 != t2 || t1 != Type::Integer {
                return Err(InferenceError::BinaryOperatorMustHaveTwoIntegers { op });
            }

            Ok((t1, env))
        }
    }
}

#[cfg(test)]
mod inference_tests {
    use super::*;

    #[test]
    fn infer_from_integer() {
        let env = Default::default();
        // 42
        let result = infer(Expr::Integer(42), env);
        assert!(result.is_ok());

        let (t, _e) = result.unwrap();
        assert_eq!(Type::Integer, t);
    }

    #[test]
    fn infer_from_boolean_literal() {
        let env = Default::default();
        // true
        let result = infer(Expr::True, env);
        assert!(result.is_ok());

        let (t, _e) = result.unwrap();
        assert_eq!(Type::Boolean, t);
    }

    #[test]
    fn infer_from_binary_operation() {
        let env = Default::default();
        // 20 + 50
        let result = infer(
            Expr::BinOp(Operator::Plus, &Expr::Integer(20), &Expr::Integer(50)),
            env,
        );
        assert!(result.is_ok());

        let (t, _e) = result.unwrap();
        assert_eq!(Type::Integer, t);
    }

    #[test]
    fn infer_from_if_else() {
        let env = Default::default();
        let result = infer(
            Expr::IfElse(&Expr::True, &Expr::Integer(2), &Expr::Integer(3)),
            env,
        );
        assert!(result.is_ok());

        let (t, _e) = result.unwrap();
        assert_eq!(Type::Integer, t);
    }

    #[test]
    fn infer_from_let_expr() {
        let env = Default::default();
        // let x = 42 in x + 3
        let result = infer(
            Expr::Let(
                "x",
                &Expr::Integer(42),
                &Expr::BinOp(Operator::Plus, &Expr::Variable("x"), &Expr::Integer(3)),
            ),
            env,
        );
        assert!(result.is_ok());

        let (t, env) = result.unwrap();
        assert_eq!(Type::Integer, t);
        assert!(env.env.contains_key("x"));

        let t = env.env.get("x").unwrap();
        assert_eq!(Type::Integer, *t);
    }

    #[test]
    fn infer_from_nested_let() {
        let env = Default::default();
        // let x = 42 in let y = 43 in x * y
        let result = infer(
            Expr::Let(
                "x",
                &Expr::Integer(42),
                &Expr::Let(
                    "y",
                    &Expr::Integer(43),
                    &Expr::BinOp(
                        Operator::Multiply,
                        &Expr::Variable("x"),
                        &Expr::Variable("y"),
                    ),
                ),
            ),
            env,
        );
        assert!(result.is_ok());

        let (t, env) = result.unwrap();
        assert_eq!(Type::Integer, t);
        assert!(env.env.contains_key("x") && env.env.contains_key("y"));

        let t = env.env.get("x").unwrap();
        assert_eq!(Type::Integer, *t);
        let t = env.env.get("y").unwrap();
        assert_eq!(Type::Integer, *t);
    }
}
