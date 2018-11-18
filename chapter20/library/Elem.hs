module Elem where

import Prelude hiding (elem)

elem :: (Foldable t, Eq a)
     => a -> t a -> Bool
elem a = foldr f False
  where f x acc = (x == a) || acc

main :: IO ()
main = do
  print $ elem 1 []
  print $ elem 5000 [1..10000]
  print $ elem 1 Nothing
  print $ elem 1 (Just 1)
  print $ elem 1 (2, 3)
  print $ elem 1 (2, 1)
