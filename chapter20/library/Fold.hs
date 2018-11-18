module Fold where

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id

main :: IO ()
main = do
  print (fold [] :: String)
  print $ fold ["hello", " world", "!"]
  print (fold Nothing :: String)
  print $ fold $ Just "ab"
  print $ fold ("ab", "cd")
