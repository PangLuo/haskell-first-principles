module Length where

import Prelude hiding (length)
import Data.Monoid

length :: (Foldable t) => t a -> Int
length = getSum . foldMap (const $ Sum 1)

main :: IO ()
main = do
  print $ length []
  print $ length [1..100000]
  print $ length Nothing
  print $ length $ Just 2
  print $ length (2, 3)
