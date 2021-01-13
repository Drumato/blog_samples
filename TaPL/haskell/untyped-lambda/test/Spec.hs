import Eval
import Syntax
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Evaluator Tests"
    [ -- E(\x -> x) = \x -> x
      testCase "Simple Abstraction" $
        eval [] (TAbs "x" (TVar 0 0)) @?= Just (TAbs "x" (TVar 0 0)),
      -- E(\x->(\f->f x) (\x -> x)) = \f -> f (\x -> x)
      testCase "Simple Application" $
        eval [] (TApp (TAbs "x" (TAbs "f" (TApp (TVar 0 2) (TVar 1 2)))) (TAbs "x" (TVar 0 1))) @?= Just (TAbs "f" (TApp (TVar 0 1) (TAbs "x" (TVar 0 2))))
    ]