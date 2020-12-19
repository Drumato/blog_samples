use std::{collections::HashMap, ops::RangeInclusive};
use thiserror::Error;
use typed_arena::Arena;

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

pub fn main<'a>(alloc: &'a Arena<Type<'a>>, e: Expr<'a>) -> Result<Type<'a>, InferenceError> {
    let env = Default::default();
    let iter = 'a'..='z';
    let (_, _iter, t) = infer(env, iter, alloc, e)?;

    Ok(t)
}

/// 型推論の本体
/// envは関数の束縛変数のみならず，型変数に割り当てられた実際の型も格納するので注意．
fn infer<'a>(
    mut env: HashMap<String, Type<'a>>,
    mut iter: RangeInclusive<char>,
    alloc: &'a Arena<Type<'a>>,
    e: Expr<'a>,
) -> Result<(HashMap<String, Type<'a>>, RangeInclusive<char>, Type<'a>), InferenceError> {
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
            let (mut env, iter, e1_t) = infer(env, iter, alloc, e1.clone())?;
            env.insert(x, e1_t);
            infer(env, iter, alloc, e2.clone())
        }
        Expr::Lambda(var, expr) => {
            let new_type_var = iter.next().unwrap().to_string();
            let new_type_var = Type::Variable(new_type_var);
            env.insert(var.to_string(), new_type_var.clone());

            let (env, iter, expr_ty) = infer(env, iter, alloc, expr.clone())?;
            let new_type_var = alloc.alloc(new_type_var);
            let expr_ty = alloc.alloc(expr_ty);
            Ok((env, iter, Type::Fn(new_type_var, expr_ty)))
        }
        Expr::Application(f, param) => {
            let new_type_var_name = iter.next().unwrap().to_string();
            let new_type_var = Type::Variable(new_type_var_name.clone());

            let (env, iter, fn_ty) = infer(env, iter, alloc, f.clone())?;
            let (env, iter, param_ty) = infer(env, iter, alloc, param.clone())?;
            let param_ty_ref = alloc.alloc(param_ty.clone());
            let new_type_var = alloc.alloc(new_type_var);
            let env = unify(env, fn_ty, Type::Fn(param_ty_ref, new_type_var))?;
            if let Some(resolved_ty) = env.get(&new_type_var_name) {
                return Ok((env.clone(), iter, resolved_ty.clone()));
            }

            Ok((env, iter, param_ty))
        }
        Expr::Plus(lhs, rhs) => {
            let (env, iter, lhs_ty) = infer(env, iter, alloc, lhs.clone())?;
            let (env, iter, rhs_ty) = infer(env, iter, alloc, rhs.clone())?;
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
fn unify<'a>(
    mut env: HashMap<String, Type<'a>>,
    t1: Type<'a>,
    t2: Type<'a>,
) -> Result<HashMap<String, Type<'a>>, InferenceError> {
    match (t1.clone(), t2.clone()) {
        // シンプルな比較
        (Type::Integer, Type::Integer) | (Type::Boolean, Type::Boolean) => Ok(env),
        (Type::Fn(var_ty1, ret_ty1), Type::Fn(var_ty2, ret_ty2)) => {
            let env = unify(env, var_ty1.clone(), var_ty2.clone())?;
            unify(env, ret_ty1.clone(), ret_ty2.clone())
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

fn occur<'a>(unknown_var: &'a str, t: &Type<'a>) -> bool {
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
        let alloc = Default::default();
        let result = infer(env, iter, &alloc, Expr::Integer(42));
        assert!(result.is_ok());

        let (_env, _iter, t) = result.unwrap();
        assert_eq!(Type::Integer, t);
    }

    #[test]
    fn from_boolean_test<'a>() {
        let env = Default::default();
        let iter = 'a'..='z';
        let alloc = Default::default();
        let result = infer(env, iter, &alloc, Expr::Boolean(true));
        assert!(result.is_ok());

        let (_env, _iter, t) = result.unwrap();
        assert_eq!(Type::Boolean, t);
    }

    #[test]
    fn from_simple_let_test<'a>() {
        let env = Default::default();
        let iter = 'a'..='z';
        let alloc = Default::default();
        // let x = 3 in x
        let x = Expr::Variable("x".to_string());
        let result = infer(
            env,
            iter,
            &alloc,
            Expr::Let("x".to_string(), &Expr::Integer(3), &x),
        );
        assert!(result.is_ok());

        let (_env, _iter, t) = result.unwrap();
        assert_eq!(Type::Integer, t);
    }

    #[test]
    fn from_simple_fn_expr() {
        let env = Default::default();
        let iter = 'a'..='z';
        let alloc = Default::default();
        // \x -> x
        let x = Expr::Variable("x".to_string());
        let fn_expr = Expr::Lambda("x".to_string(), &x);

        let result = infer(env, iter, &alloc, fn_expr);
        assert!(result.is_ok());

        let (_env, _iter, t) = result.unwrap();
        assert_eq!(
            Type::Fn(
                &Type::Variable("a".to_string()),
                &Type::Variable("a".to_string())
            ),
            t
        );
    }

    #[test]
    fn from_simple_fn_application() {
        let env = Default::default();
        let iter = 'a'..='z';
        let alloc = Default::default();

        // ((\x -> x) 3)
        let x = Expr::Variable("x".to_string());
        let fn_expr = Expr::Lambda("x".to_string(), &x);
        let apply_expr = Expr::Application(&fn_expr, &Expr::Integer(3));

        let result = infer(env, iter, &alloc, apply_expr);
        assert!(result.is_ok());

        let (env, iter, t) = result.unwrap();
        assert_eq!(Type::Integer, t);

        // ((\x -> x + 4) 3)
        let plus_expr = Expr::Plus(&x, &Expr::Integer(4));
        let fn_expr = Expr::Lambda("x".to_string(), &plus_expr);
        let apply_expr = Expr::Application(&fn_expr, &Expr::Integer(3));

        let result = infer(env, iter, &alloc, apply_expr);
        assert!(result.is_ok());

        let (_env, _iter, t) = result.unwrap();
        assert_eq!(Type::Integer, t);
    }

    #[test]
    fn from_nested_fn_application() {
        let env = Default::default();
        let iter = 'a'..='z';
        let alloc = Default::default();

        // (((\x -> (\y -> x + y)) 3) 4)
        let x = Expr::Variable("x".to_string());
        let y = Expr::Variable("y".to_string());
        let plus_expr = Expr::Plus(&x, &y);
        let inner_fn_expr = Expr::Lambda("y".to_string(), &plus_expr);
        let outer_fn_expr = Expr::Lambda("x".to_string(), &inner_fn_expr);
        let inner_apply_expr = Expr::Application(&outer_fn_expr, &Expr::Integer(3));
        let outer_apply_expr = Expr::Application(&inner_apply_expr, &Expr::Integer(4));

        let result = infer(env, iter, &alloc, outer_apply_expr);
        assert!(result.is_ok());

        let (_env, _iter, t) = result.unwrap();
        assert_eq!(Type::Integer, t);
    }
}
