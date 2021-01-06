import Eval
import Syntax
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Evaluator Tests"
    [ testCase "Constant True" $
        eval TermTrue @?= Right TermTrue,
      testCase "Constant False" $
        eval TermFalse @?= Right TermFalse,
      testCase "Simple IsZero1" $
        eval (TermIsZero TermZero)
          @?= Right TermTrue,
      testCase
        "Simple IsZero2"
        $ eval
          (TermIsZero (TermSucc TermZero))
          @?= Right TermFalse,
      testCase "Pred and Succ" $
        eval (TermPred (TermSucc TermZero)) @?= Right TermZero,
      testCase "Simple If" $ eval (TermIf TermTrue TermTrue TermFalse) @?= Right TermTrue,
      testCase "Simple If2" $ eval (TermIf TermFalse TermTrue TermFalse) @?= Right TermFalse,
      testCase "invalid succ" $ eval (TermSucc TermFalse) @?= Left NormalizedButIsNotValue,
      testCase "runEvaluator1" $ runEvaluator TermZero @?= Right (ValueInteger 0),
      testCase "runEvaluator2" $ runEvaluator (TermSucc (TermSucc (TermSucc TermZero))) @?= Right (ValueInteger 3)
    ]