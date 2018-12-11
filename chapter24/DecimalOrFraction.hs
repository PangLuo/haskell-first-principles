module DecimalOrFraction where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

type DecimalOrFraction =
  Either Integer Rational

goodDecimal1 = "10"
goodDecimal2 = "10abc"
badFraction1 = "1/"
badFraction2 = "1/abc"
badFraction3 = "1/0"
badFraction4 = "1/0abc"
goodFraction1 = "1/2"
goodFraction2 = "1/2abc"
noNum = "abc"

fraction :: Parser Rational
fraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

parseDof :: Parser DecimalOrFraction
parseDof =
  (try (Left <$> decimal <* notFollowedBy (char '/')) <?> "Tried decimal")
    <|> (Right <$> fraction <?> "Tried fraction")

main :: IO ()
main = do
  let parseDof' = parseString parseDof mempty
  print $ parseDof' goodDecimal1
  print $ parseDof' goodDecimal2
  print $ parseDof' badFraction1
  print $ parseDof' badFraction2
  print $ parseDof' badFraction3
  print $ parseDof' badFraction4
  print $ parseDof' goodFraction1
  print $ parseDof' goodFraction2
  print $ parseDof' noNum
