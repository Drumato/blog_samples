module Main where

import Eval
import Syntax

main :: IO ()
main = print $ eval $ TermIf (TermIsZero TermZero) TermTrue TermFalse