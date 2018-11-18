module FoldMap where

import Data.Monoid
import Prelude hiding (foldMap)

foldMap :: (Foldable t, Monoid m)
        => (a -> m) -> t a -> m
foldMap f = foldr g mempty
  where g x acc = f x <> acc

main :: IO ()
main = do
  print $ foldMap Sum []
  print $ foldMap Sum [1..100]
  print $ foldMap Sum Nothing
  print $ foldMap Sum (Just 2)
  print $ foldMap Sum (2, 3)
