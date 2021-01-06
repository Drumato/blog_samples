module Eval (eval, runEvaluator, Value (ValueTrue, ValueFalse, ValueInteger), EvalError (NoRuleApplies, NormalizedButIsNotValue)) where

import Syntax

data EvalError = NoRuleApplies | NormalizedButIsNotValue deriving (Eq, Show)

data Value = ValueTrue | ValueFalse | ValueInteger Int deriving (Eq, Show)

runEvaluator :: Term -> Either EvalError Value
runEvaluator t = case eval t of
  Right t' -> Right $ toValue t
  Left e -> Left e

eval :: Term -> Either EvalError Term
eval t = case eval1 t of
  Right t' -> eval t'
  Left e -> if isValue t then Right t else Left NormalizedButIsNotValue

eval1 :: Term -> Either EvalError Term
eval1 t = case t of
  -- 条件部が定数ブール値の場合、単一ステップで評価可能
  TermIf TermTrue t2 t3 -> Right t2
  TermIf TermFalse t2 t3 -> Right t3
  -- If項全体の評価までは進まない
  TermIf t1 t2 t3 -> case eval1 t1 of
    Right t1' -> Right $ TermIf t1' t2 t3
    e -> e
  --
  TermSucc t1 -> case eval1 t1 of
    Right t1' -> Right $ TermSucc t1'
    e -> e
  TermPred TermZero -> Right TermZero
  TermPred (TermSucc t1) | isNumericValue t1 -> Right t1
  TermPred t1 -> case eval1 t1 of
    Right t1' -> Right $ TermPred t1'
    e -> e
  TermIsZero TermZero -> Right TermTrue
  TermIsZero (TermSucc t1) | isNumericValue t1 -> Right TermFalse
  TermIsZero t1 -> case eval1 t1 of
    Right t1' -> Right $ TermIsZero t1
    e -> e
  _ -> Left NoRuleApplies

toValue :: Term -> Value
toValue TermTrue = ValueTrue
toValue TermFalse = ValueFalse
toValue TermZero = ValueInteger 0
toValue (TermSucc t1) = case toValue t1 of
  ValueInteger n -> ValueInteger (n + 1)
  _ -> error "unreachable"

-- | 項が数値であるかどうかの判定
isNumericValue :: Term -> Bool
isNumericValue t = case t of
  TermZero -> True
  TermSucc t1 -> isNumericValue t1
  _ -> False

-- | 項が値であるかどうかの判定
isValue :: Term -> Bool
isValue t = case t of
  TermTrue -> True
  TermFalse -> True
  x -> isNumericValue x
