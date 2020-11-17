import Test.Tasty as Tasty
import Test.Tasty.HUnit as HUnit
import Parser as P

main :: IO ()
main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests = Tasty.testGroup "Parser Tests" [ characterTests ]

characterTests :: Tasty.TestTree
characterTests = Tasty.testGroup "character Tests" [
    HUnit.testCase "simple character" $
        HUnit.assertEqual "" (Right ("bc", 'a')) (P.character 'a' "abc")
    , HUnit.testCase "empty input" $
    HUnit.assertEqual "" (Left P.Character) (P.character 'a' "")
    ]