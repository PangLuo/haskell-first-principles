module AnotherThree where

data Three' a b =
  Three' a b b

instance Foldable (Three' a) where
  foldMap f (Three' x y z) = f y <> f z
