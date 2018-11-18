module ToList where

toList :: (Foldable t) => t a -> [a]
toList = foldr f []
  where f x acc = x:acc

main :: IO ()
main = do
  print (toList [] :: [Int])
  print $ toList [1..100]
  print (toList Nothing :: [Int])
  print $ toList $ Just 2
  print $ toList (2, 3)
