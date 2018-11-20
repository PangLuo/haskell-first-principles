module Pair where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Pair a b =
  Pair a b
  deriving (Eq, Show)

instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)

instance Foldable (Pair a) where
  foldMap f (Pair a b) = f b

instance Traversable (Pair a) where
  traverse f (Pair a b) = Pair a <$> f b

instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Pair a b) where
  arbitrary = Pair <$> arbitrary <*> arbitrary

trigger :: Pair Int (String, String, String)
trigger = undefined

main :: IO ()
main = do
  quickBatch (functor trigger)
  quickBatch (traversable trigger)
