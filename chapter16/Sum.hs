module Sum where

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second b) = Second (f b)
  fmap _ (First a) = First a

-- A Functor instance that applies the function only to First, Either's Left
-- is impossible because "First a" or "Left a" is part of the structure that
-- can't be touched.
