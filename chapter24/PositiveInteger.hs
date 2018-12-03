module PositiveInteger where

import Control.Applicative
import Data.Char
import Text.Trifecta

parseDigit :: Parser Char
parseDigit = oneOf ['0'..'9'] <?> "parseDigit"

base10Integer :: Parser Integer
base10Integer = go 0 where
  go acc = do
    d <- parseDigit <?> "integer"
    let acc' = acc * 10 + toInteger (digitToInt d)
    try (go acc') <|> return acc'

base10Integer' :: Parser Integer
base10Integer' =
  (try base10Integer) <|> (char '-' >> (0-) <$> base10Integer)
