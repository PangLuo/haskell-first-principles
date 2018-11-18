module Minimum where

import Prelude hiding (minimum)

minimum :: (Foldable t, Ord a)
        => t a -> Maybe a
minimum = foldr f Nothing
  where f x acc =
          case acc of
            Just a -> Just $ min x a
            _ -> Just x

main :: IO ()
main = do
  print (minimum [] :: Maybe Int)
  print $ minimum [1..100000]
  print (minimum Nothing :: Maybe Int)
  print $ minimum $ Just 100
  print $ minimum (2, 3)
