module LearnParsers where

import Text.Parser.Combinators
import Text.Trifecta
import Data.List

stop :: Parser a
stop = unexpected "stop"

one = char '1'

one' = one >> stop

oneTwo = char '1' >> char '2'

oneTwo' = oneTwo >> stop

oneTwoThree :: Parser String
oneTwoThree = choice [string "123", string "12", string "1"]

string' :: String -> Parser String
string' "" = return ""
string' (x:xs) =
  char x >>= \x -> string' xs >>= \xs -> return (x:xs)

pNL s =
  putStrLn ('\n': s)

main = do
  pNL "one eof:"
  print $ parseString (one >> eof) mempty "123"

  pNL "oneTwo eof:"
  print $ parseString (oneTwo >> eof) mempty "123"

  pNL "oneTwoThree:"
  print $ parseString oneTwoThree mempty "1"
  print $ parseString oneTwoThree mempty "12"
  print $ parseString oneTwoThree mempty "123"

  pNL "string:"
  print $ parseString (string "123") mempty "123"

  pNL "string':"
  print $ parseString (string' "123") mempty "123"
