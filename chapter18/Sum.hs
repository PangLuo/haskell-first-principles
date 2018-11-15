module Sum where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

instance Applicative (Sum a) where
  pure = Second
  First e <*> _ = First e
  Second f <*> r = fmap f r

instance Monad (Sum a) where
  return = pure
  First e >>= _ = First e
  Second r >>= k = k r

instance (Eq a, Eq b) => EqProp (Sum a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Sum a b) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 elements [First a, Second b]

trigger :: Sum String (String, String, Int)
trigger = undefined

main :: IO ()
main = do
  quickBatch (functor trigger)
  quickBatch (applicative trigger)
  quickBatch (monad trigger)
