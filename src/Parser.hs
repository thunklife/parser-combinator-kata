module Parser where

newtype Parser a = Parser (String -> Either String (a, String))

pChar :: Char -> Parser Char
pChar c = Parser pChar'
  where 
    pChar' [] = Left "Empty input"
    pChar' s
      | head s == c = Right (c, drop 1 s)
      | otherwise = Left "Not Found"

run :: Parser a -> String -> Either String (a, String)
run (Parser fn) = fn

(.>>.) :: Parser a -> Parser b -> Parser (a, b)
(Parser pa) .>>. (Parser pb) = Parser andThen
  where
    andThen s =
      case pa s of
        (Left err) -> Left err
        (Right (c1, rest1)) ->
          case pb rest1 of 
            (Left err) -> Left err
            (Right (c2, rest2)) -> Right ((c1, c2), rest2)

(<|>) :: Parser a -> Parser a -> Parser a
(Parser pa) <|> (Parser pb) = Parser orElse 
  where
    orElse s =
      case pa s of
        (Left _) ->
          case pb s of 
            (Left err) -> Left err
            res2 -> res2 
        res1 -> res1