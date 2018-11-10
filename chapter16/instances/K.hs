module K where

data K a b = K a
  deriving (Eq, Show)

instance Functor (K a) where
  fmap f (K a) = K a
