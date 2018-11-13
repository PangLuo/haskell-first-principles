module A where

added :: Maybe Integer
added =
  fmap (+3) (lookup 3 $ zip [1, 2, 3] [4, 5, 6])
