module Four where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) =>
         Applicative (Four a b c) where
  pure = Four mempty mempty mempty
  Four x y z f <*> Four x' y' z' a =
    Four (x <> x') (y <> y') (z <> z') (f a)

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 c <- arbitrary
                 d <- arbitrary
                 return $ Four a b c d

trigger :: Four String Ordering String (String, Ordering, Int)
trigger = undefined

main :: IO ()
main = do
  quickBatch (functor trigger)
  quickBatch (applicative trigger)
