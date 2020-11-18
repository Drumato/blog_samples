{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}

module Parser (character, tag, satisfy, ParseError (Character, Tag, Satisfy)) where

import Data.List

data ParseError
  = Character
  | Tag
  | Satisfy
  deriving (Eq, Show)

character :: Char -> String -> Either ParseError (String, Char)
character pat = \case
  (x : xs) -> if x == pat then Right (xs, x) else Left Character
  _ -> Left Character

tag :: String -> String -> Either ParseError (String, String)
tag pat input = case stripPrefix pat input of
  Just s -> Right (s, pat)
  Nothing -> Left Tag

satisfy :: (Char -> Bool) -> String -> Either ParseError (String, Char)
satisfy cond = \case
  (x : xs) -> if cond x then Right (xs, x) else Left Satisfy
  _ -> Left Satisfy