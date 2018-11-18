module Three where

data Three a b c =
  Three a b c

instance Foldable (Three a b) where
  foldMap f (Three _ _ c) = f c
