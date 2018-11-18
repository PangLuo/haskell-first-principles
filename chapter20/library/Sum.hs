module Sum where

import Data.Monoid
import Prelude hiding (sum)

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

main :: IO ()
main = do
  print $ sum []
  print $ sum [1..100]
  print $ sum Nothing
  print $ sum $ Just 4
  print $ sum (1, 2)
