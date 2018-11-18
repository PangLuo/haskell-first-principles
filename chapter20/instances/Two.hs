module Two where

data Two a b =
  Two a b

instance Foldable (Two a) where
  foldMap f (Two _ b) = f b
