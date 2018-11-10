module Check where

import Test.QuickCheck

functorIdentity :: (Functor f, Eq (f a)) =>
                   f a -> Bool
functorIdentity f =
  fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
                     f a
                  -> Fun a b
                  -> Fun b c
                  -> Bool
functorCompose x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)
