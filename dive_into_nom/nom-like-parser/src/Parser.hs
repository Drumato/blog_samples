{-# LANGUAGE LambdaCase #-}

module Parser (character, tag, ParseError (Character, Tag)) where

import Data.List

data ParseError
  = Character
  | Tag
  deriving (Eq, Show)

character :: Char -> String -> Either ParseError (String, Char)
character pat = \case
  (x : xs) -> if x == pat then Right (xs, x) else Left Character
  _ -> Left Character

tag :: String -> String -> Either ParseError (String, String)
tag pat input = case stripPrefix pat input of
  Just s -> Right (s, pat)
  Nothing -> Left Tag