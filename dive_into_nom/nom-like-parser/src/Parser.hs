module Parser (character, ParseError(Character)) where

data ParseError = Character
    deriving (Eq, Show)

character :: Char -> String -> Either ParseError (String, Char)
character pat input = case input of
    (x: xs) -> if pat == x then Right (xs, x) else Left Character
    _ -> Left Character