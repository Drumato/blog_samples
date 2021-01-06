module Syntax
  ( Term (TermTrue, TermFalse, TermZero, TermSucc, TermPred, TermIsZero, TermIf),
  )
where

data Term
  = TermTrue -- "true"
  | TermFalse -- "false"
  | TermZero -- "zero"
  | TermSucc Term -- "succ" t
  | TermPred Term -- "pred" t
  | TermIsZero Term -- "iszero" t
  | TermIf Term Term Term -- "if" t1 "then" t2 "else" t3
  deriving (Show, Eq)