module Syntax (Term (TVar, TAbs, TApp), Context, Binding, isValue) where

data Term
  = TVar Int Int -- 変数． De brujinインデックスと，一番外側のλ抽象までの距離
  | TAbs String Term
  | -- | λ抽象． 文字列は明瞭なメッセージ用で，本質は項
    TApp Term Term -- 関数適用．
  deriving (Eq)

instance Show Term where
  show t = case t of
    TVar x c -> "var" ++ show x ++ ": " ++ show c
    TAbs x t1 -> "λ" ++ x ++ "." ++ show t1
    TApp t1 t2 -> "(" ++ show t1 ++ " " ++ show t2 ++ ")"

type Context = [(String, Binding)]

type Binding = ()

isValue :: Context -> Term -> Bool
isValue _ t = case t of
  TAbs _ _ -> True
  _ -> False