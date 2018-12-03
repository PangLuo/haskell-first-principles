module ParseIntegerEOF where

import Text.Trifecta

parseIntegerEOF :: Parser Integer
parseIntegerEOF = do
  i <- integer
  eof
  return i
