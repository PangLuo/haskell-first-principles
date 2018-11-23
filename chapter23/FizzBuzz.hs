module FizzBuzz where

import Control.Monad
import Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n
  | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

fizzbuzzFromTo :: Integer
               -> Integer
               -> [String]
fizzbuzzFromTo a b =
  execState (mapM_ addResult [b, b-1 .. a]) []

addResult :: Integer -> State [String] ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  put (result : xs)

main :: IO ()
main =
  mapM_ putStrLn $
    fizzbuzzFromTo 1 100
