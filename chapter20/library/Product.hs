module Product where

import Data.Monoid
import Prelude hiding (product)

product :: (Foldable t, Num a) => t a -> a
product = getProduct . foldMap Product

main :: IO ()
main = do
  print $ product []
  print $ product [1..3]
  print $ product Nothing
  print $ product $ Just 4
  print $ product (2, 3)
