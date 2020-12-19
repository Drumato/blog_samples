use std::{collections::HashMap, ops::RangeInclusive};
use thiserror::Error;

use crate::{expr::Expr, types::Type};

#[derive(Debug, Error)]
pub enum InferenceError {
    #[error("not found such a variable '{v:?}' in type-env")]
    NotFoundSuchAVariable { v: String },
    #[error("found an occurrence")]
    FoundOccurrence,
    #[error("cannot unify")]
    CannotUnify,
}

pub fn main(e: Expr) -> Result<Type, InferenceError> {
    let env = Default::default();
    let iter = 'a'..='z';
    let (_, _iter, t) = infer(env, iter, e)?;

    Ok(t)
}

/// 型推論の本体
/// envは関数の束縛変数のみならず，型変数に割り当てられた実際の型も格納するので注意．
fn infer(
    mut env: HashMap<String, Type>,
    mut iter: RangeInclusive<char>,
    e: Expr,
) -> Result<(HashMap<String, Type>, RangeInclusive<char>, Type), InferenceError> {
    match e {
        Expr::Boolean(_b) => Ok((env, iter, Type::Boolean)),
        Expr::Integer(_v) => Ok((env, iter, Type::Integer)),
        Expr::Variable(name) => match env.get(&name) {
            Some(var_ty) => Ok((env.clone(), iter, var_ty.clone())),
            None => Err(InferenceError::NotFoundSuchAVariable {
                v: name.to_string(),
            }),
        },
        Expr::Let(x, e1, e2) => {
            let (mut env, iter, e1_t) = infer(env, iter, *e1)?;
            env.insert(x, e1_t);
            infer(env, iter, *e2)
        }
        Expr::Lambda(var, expr) => {
            let new_type_var = iter.next().unwrap().to_string();
            let new_type_var = Type::Variable(new_type_var);
            env.insert(var.to_string(), new_type_var.clone());

            let (env, iter, expr_ty) = infer(env, iter, *expr)?;
            Ok((
                env,
                iter,
                Type::Fn(Box::new(new_type_var), Box::new(expr_ty)),
            ))
        }
        Expr::Application(f, param) => {
            let new_type_var_name = iter.next().unwrap().to_string();
            let new_type_var = Type::Variable(new_type_var_name.clone());

            let (env, iter, fn_ty) = infer(env, iter, *f)?;
            let (env, iter, param_ty) = infer(env, iter, *param)?;
            let env = unify(
                env,
                fn_ty,
                Type::Fn(Box::new(param_ty.clone()), Box::new(new_type_var)),
            )?;
            if let Some(resolved_ty) = env.get(&new_type_var_name) {
                return Ok((env.clone(), iter, resolved_ty.clone()));
            }

            Ok((env, iter, param_ty))
        }
        Expr::Plus(lhs, rhs) => {
            let (env, iter, lhs_ty) = infer(env, iter, *lhs)?;
            let (env, iter, rhs_ty) = infer(env, iter, *rhs)?;
            let env = unify(env, Type::Integer, lhs_ty.clone())?;
            let env = unify(env, Type::Integer, rhs_ty.clone())?;

            if let (Type::Variable(var), _) = (&lhs_ty, &rhs_ty) {
                let resolved_ty = env.get(var).unwrap().clone();
                return Ok((env, iter, resolved_ty));
            }
            if let (_, Type::Variable(var)) = (&lhs_ty, &rhs_ty) {
                let resolved_ty = env.get(var).unwrap().clone();
                return Ok((env, iter, resolved_ty));
            }
            Ok((env, iter, lhs_ty))
        }
    }
}

/// 型の比較，単一化
fn unify(
    mut env: HashMap<String, Type>,
    t1: Type,
    t2: Type,
) -> Result<HashMap<String, Type>, InferenceError> {
    match (t1.clone(), t2.clone()) {
        // シンプルな比較
        (Type::Integer, Type::Integer) | (Type::Boolean, Type::Boolean) => Ok(env),
        (Type::Fn(var_ty1, ret_ty1), Type::Fn(var_ty2, ret_ty2)) => {
            let env = unify(env, *var_ty1, *var_ty2)?;
            unify(env, *ret_ty1, *ret_ty2)
        }
        (Type::Variable(name1), Type::Variable(name2)) if name1 == name2 => Ok(env),

        // 一方が型変数の場合を調べる
        (Type::Variable(var), _) => {
            // 定義済み(割り当て済み)の場合，単純比較
            if let Some(var_t) = env.get(&var) {
                return unify(env.clone(), var_t.clone(), t2);
            }

            // 未定義(未知)の場合，occur check後代入
            if occur(&var, &t2) {
                return Err(InferenceError::FoundOccurrence);
            }

            env.insert(var.to_string(), t2);
            Ok(env)
        }
        (_, Type::Variable(var)) => {
            if let Some(var_t) = env.get(&var) {
                return unify(env.clone(), var_t.clone(), t1);
            }
            if occur(&var, &t1) {
                return Err(InferenceError::FoundOccurrence);
            }

            env.insert(var.to_string(), t1);
            Ok(env)
        }
        _ => Err(InferenceError::CannotUnify),
    }
}

fn occur<'a>(unknown_var: &'a str, t: &'a Type) -> bool {
    match t {
        // 引数か返り値のどちらかに型変数が登場したら代入不可．無限ループに陥る
        Type::Fn(var_ty, ret_ty) => occur(unknown_var, var_ty) || occur(unknown_var, ret_ty),

        Type::Variable(v2) => unknown_var == v2,
        _ => false,
    }
}

#[cfg(test)]
mod inference_tests {
    use super::*;

    #[test]
    fn from_integer_test<'a>() {
        let env = Default::default();
        let iter = 'a'..='z';
        let result = infer(env, iter, Expr::Integer(42));
        assert!(result.is_ok());

        let (_env, _iter, t) = result.unwrap();
        assert_eq!(Type::Integer, t);
    }

    #[test]
    fn from_boolean_test<'a>() {
        let env = Default::default();
        let iter = 'a'..='z';
        let result = infer(env, iter, Expr::Boolean(true));
        assert!(result.is_ok());

        let (_env, _iter, t) = result.unwrap();
        assert_eq!(Type::Boolean, t);
    }

    #[test]
    fn from_simple_let_test<'a>() {
        let env = Default::default();
        let iter = 'a'..='z';
        // let x = 3 in x
        let x = Expr::Variable("x".to_string());
        let result = infer(
            env,
            iter,
            Expr::Let("x".to_string(), Box::new(Expr::Integer(3)), Box::new(x)),
        );
        assert!(result.is_ok());

        let (_env, _iter, t) = result.unwrap();
        assert_eq!(Type::Integer, t);
    }

    #[test]
    fn from_nested_let_expr() {
        let env = Default::default();
        let iter = 'a'..='z';
        // let x = 3 in let y = x in y
        let x = Expr::Variable("x".to_string());
        let y = Expr::Variable("y".to_string());
        let let_expr = Expr::Let("y".to_string(), Box::new(x.clone()), Box::new(y));
        let let_expr = Expr::Let("x".to_string(), Box::new(Expr::Integer(3)), Box::new(let_expr));
        let result = infer(
            env,
            iter,
            let_expr,
        );
        assert!(result.is_ok());

        let (_env, _iter, t) = result.unwrap();
        assert_eq!(Type::Integer, t);
    }

    #[test]
    fn from_simple_fn_expr() {
        let env = Default::default();
        let iter = 'a'..='z';
        // \x -> x
        let x = Expr::Variable("x".to_string());
        let fn_expr = Expr::Lambda("x".to_string(), Box::new(x));

        let result = infer(env, iter, fn_expr);
        assert!(result.is_ok());

        let (_env, _iter, t) = result.unwrap();
        assert_eq!(
            Type::Fn(
                Box::new(Type::Variable("a".to_string())),
                Box::new(Type::Variable("a".to_string()))
            ),
            t
        );
    }

    #[test]
    fn from_simple_fn_application() {
        let env = Default::default();
        let iter = 'a'..='z';

        // ((\x -> x) 3)
        let x = Expr::Variable("x".to_string());
        let fn_expr = Expr::Lambda("x".to_string(), Box::new(x.clone()));
        let apply_expr = Expr::Application(Box::new(fn_expr), Box::new(Expr::Integer(3)));

        let result = infer(env, iter, apply_expr);
        assert!(result.is_ok());

        let (env, iter, t) = result.unwrap();
        assert_eq!(Type::Integer, t);

        // ((\x -> x + 4) 3)
        let plus_expr = Expr::Plus(Box::new(x), Box::new(Expr::Integer(4)));
        let fn_expr = Expr::Lambda("x".to_string(), Box::new(plus_expr));
        let apply_expr = Expr::Application(Box::new(fn_expr), Box::new(Expr::Integer(3)));

        let result = infer(env, iter, apply_expr);
        assert!(result.is_ok());

        let (_env, _iter, t) = result.unwrap();
        assert_eq!(Type::Integer, t);
    }

    #[test]
    fn from_nested_fn_application() {
        let env = Default::default();
        let iter = 'a'..='z';

        // (((\x -> (\y -> x + y)) 3) 4)
        let x = Expr::Variable("x".to_string());
        let y = Expr::Variable("y".to_string());
        let plus_expr = Expr::Plus(Box::new(x), Box::new(y));
        let inner_fn_expr = Expr::Lambda("y".to_string(), Box::new(plus_expr));
        let outer_fn_expr = Expr::Lambda("x".to_string(), Box::new(inner_fn_expr));
        let inner_apply_expr =
            Expr::Application(Box::new(outer_fn_expr), Box::new(Expr::Integer(3)));
        let outer_apply_expr =
            Expr::Application(Box::new(inner_apply_expr), Box::new(Expr::Integer(4)));

        let result = infer(env, iter, outer_apply_expr);
        assert!(result.is_ok());

        let (_env, _iter, t) = result.unwrap();
        assert_eq!(Type::Integer, t);
    }
}
