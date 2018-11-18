module Maximum where

import Prelude hiding (maximum)

maximum :: (Foldable t, Ord a)
        => t a -> Maybe a
maximum = foldr f Nothing
  where f x acc =
          case acc of
            Just a -> Just $ max x a
            _ -> Just x

main :: IO ()
main = do
  print (maximum [] :: Maybe Int)
  print $ maximum [1..100000]
  print (maximum Nothing :: Maybe Int)
  print $ maximum $ Just 100
  print $ maximum (2, 3)
