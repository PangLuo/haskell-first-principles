module LiftItOut where

data LiftItOut f a =
  LiftItOut (f a)
  deriving (Eq, Show)

instance Functor f =>
         Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)
