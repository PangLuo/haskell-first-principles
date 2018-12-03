module PhoneNumber where

import Control.Applicative
import Data.Char
import Text.Trifecta

type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
  PhoneNumber NumberingPlanArea
              Exchange LineNumber
  deriving (Eq, Show)

f :: Int -> Parser Int
f n = read <$> count n digit

parserA :: Parser PhoneNumber
parserA = do
  a <- f 3
  char '-'
  b <- f 3
  char '-'
  c <- f 4
  return $ PhoneNumber a b c

parserB :: Parser PhoneNumber
parserB = do
  a <- f 3
  b <- f 3
  c <- f 4
  return $ PhoneNumber a b c

parserC :: Parser PhoneNumber
parserC = do
  char '('
  a <- f 3
  string ") "
  b <- f 3
  char '-'
  c <- f 4
  return $ PhoneNumber a b c

parserD :: Parser PhoneNumber
parserD = do
  string "1-"
  a <- f 3
  char '-'
  b <- f 3
  char '-'
  c <- f 4
  return $ PhoneNumber a b c

parsePhone :: Parser PhoneNumber
parsePhone = try parserA <|> try parserB <|> try parserC <|> parserD
