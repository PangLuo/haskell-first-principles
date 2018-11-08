module Optional where

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a
      => Semigroup (Optional a) where
  Nada <> x = x
  Only x <> Only y = Only (x <> y)
  x <> _ = x

instance Monoid a
      => Monoid (Optional a) where
  mempty = Nada
