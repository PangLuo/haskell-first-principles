module Idempotence where

import Data.Char
import Data.List
import Test.QuickCheck

twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

f :: String -> Bool
f x = (capitalizeWord x == twice capitalizeWord x) &&
      (capitalizeWord x == fourTimes capitalizeWord x)

f' :: [Int] -> Bool
f' x = (sort x == twice sort x) &&
       (sort x == fourTimes sort x)

main :: IO ()
main = do
  quickCheck f
  quickCheck f'
