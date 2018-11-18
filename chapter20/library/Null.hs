module Null where

import Prelude hiding (null)

null :: (Foldable t) => t a -> Bool
null = foldr f True
  where f _ _ = False

main :: IO ()
main = do
  print $ null []
  print $ null [1..100000]
  print $ null Nothing
  print $ null $ Just [1..100000]
  print $ null (1, 2)
