import Parser as P
import Test.Tasty as Tasty
import Test.Tasty.HUnit as HUnit

main :: IO ()
main = Tasty.defaultMain tests

tests :: Tasty.TestTree
tests = Tasty.testGroup "Parser Tests" [characterTests, tagTests]

characterTests :: Tasty.TestTree
characterTests =
  Tasty.testGroup
    "character Tests"
    [ HUnit.testCase "simple character" $
        HUnit.assertEqual "" (Right ("bc", 'a')) (P.character 'a' "abc"),
      HUnit.testCase "not matching" $
        HUnit.assertEqual "" (Left P.Character) (P.character 'a' "bcd"),
      HUnit.testCase "empty input" $
        HUnit.assertEqual "" (Left P.Character) (P.character 'a' "")
    ]
tagTests :: Tasty.TestTree
tagTests =
  Tasty.testGroup
    "tag Tests"
    [ HUnit.testCase "simple tag" $
        HUnit.assertEqual "" (Right ("ato", "Drum")) (P.tag "Drum" "Drumato"),
      HUnit.testCase "not matching" $
        HUnit.assertEqual "" (Left P.Tag) (P.tag "Drum" "drumato"),
      HUnit.testCase "empty input" $
        HUnit.assertEqual "" (Left P.Tag) (P.tag "a" "")
    ]
