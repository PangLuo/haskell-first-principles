module AnotherFour where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Four' a b = Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Monoid a) => Applicative (Four' a) where
  pure = Four' mempty mempty mempty
  Four' x y z f <*> Four' x' y' z' a =
    Four' (x <> x') (y <> y') (z <> z') (f a)

instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 c <- arbitrary
                 d <- arbitrary
                 return $ Four' a b c d

trigger :: Four' String (String, Ordering, Int)
trigger = undefined

main :: IO ()
main = do
  quickBatch (functor trigger)
  quickBatch (applicative trigger)
