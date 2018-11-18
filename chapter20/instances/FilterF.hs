module FilterF where

filterF :: (Applicative f, Foldable t, Monoid (f a)) =>
           (a -> Bool) -> t a -> f a
filterF p xs = foldMap f xs
  where f x
          | p x = pure x
          | otherwise = mempty
