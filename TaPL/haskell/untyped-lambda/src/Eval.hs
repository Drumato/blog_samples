module Eval (eval) where

import qualified Data.Maybe as M
import qualified Syntax as S

-- | 項の評価器
eval :: S.Context -> S.Term -> Maybe S.Term
eval ctx t =
  if S.isValue ctx t
    then Just t
    else case eval1 ctx t of
      Just t' -> eval ctx t'
      Nothing -> Nothing

-- | 単一ステップ評価器
eval1 :: S.Context -> S.Term -> Maybe S.Term
eval1 ctx t = case t of
  S.TApp (S.TAbs x t12) t2 ->
    if S.isValue ctx t2
      then Just (substituteTop t2 t12)
      else fmap (S.TApp (S.TAbs x t12)) (eval1 ctx t2)
  S.TApp t1 t2 -> fmap (`S.TApp` t2) (eval1 ctx t1)
  _ -> Nothing

-- | (λx.M N) というλ項に対して呼び出される
-- | N内のxの出現を調べて，全てN置き換える操作を行う．
-- | まず substitute 0で一番外の変数を置き換える
-- | 一番外側のラムダ抽象が簡約されるので-1 シフトする
-- | 代入後の項sは+-0になってほしいので，先に1だけシフト
substituteTop :: S.Term -> S.Term -> S.Term
substituteTop s t = shift (-1) (substitute 0 (shift 1 s) t)

-- | 項に登場する自由変数のde brujinインデックスをdシフト
shift :: Int -> S.Term -> S.Term
shift d = walk 0
  where
    walk c t = case t of
      -- 文脈 c より xが大きい(自由変数である)場合
      S.TVar x n -> if x >= c then S.TVar (x + d) (n + d) else S.TVar x (n + d)
      S.TAbs x t1 -> S.TAbs x (walk (c + 1) t1)
      S.TApp t1 t2 -> S.TApp (walk c t1) (walk c t2)

-- | 項内のj番目の変数へ項sを代入する
substitute :: Int -> S.Term -> S.Term -> S.Term
substitute j s = walk 0
  where
    walk c t = case t of
      S.TVar x n -> if x == (j + c) then shift c s else S.TVar x n
      S.TAbs x t1 -> S.TAbs x (walk (c + 1) t1)
      S.TApp t1 t2 -> S.TApp (walk c t1) (walk c t2)