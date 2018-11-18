module Constant where

data Constant a b =
  Constant b

instance Foldable (Constant a) where
  foldMap f (Constant b) = f b
