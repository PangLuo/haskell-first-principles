module AnotherFour where

data Four' a b =
  Four' a b b b

instance Foldable (Four' a) where
  foldMap f (Four' w x y z) = f x <> f y <> f z
