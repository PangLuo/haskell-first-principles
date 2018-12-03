module DecimalOrFraction where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

type DecimalOrFraction =
  Either Integer Rational

goodDecimal = "10"
badFraction = "1/0"
goodFraction1 = "1/2"
goodFraction2 = "2/1"
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
  ((try (Right <$> fraction) <?> "Tried fraction") <|>
   (Left <$> decimal <?> "Tried decimal")) <* eof

main :: IO ()
main = do
  let parseDof' = parseString parseDof mempty
  print $ parseDof' goodDecimal
  print $ parseDof' badFraction
  print $ parseDof' goodFraction1
  print $ parseDof' goodFraction2
  print $ parseDof' noNum
