module Bigger where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Bigger a b =
  Bigger a b b b
  deriving (Eq, Show)

instance Functor (Bigger a) where
  fmap f (Bigger w x y z) = Bigger w (f x) (f y) (f z)

instance Foldable (Bigger a) where
  foldMap f (Bigger w x y z) = f x <> f y <> f z

instance Traversable (Bigger a) where
  traverse f (Bigger w x y z) = Bigger w <$> f x <*> f y <*> f z

instance (Eq a, Eq b) => EqProp (Bigger a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Bigger a b) where
  arbitrary = Bigger <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

trigger :: Bigger Int (String, String, Maybe String)
trigger = undefined

main :: IO ()
main = do
  quickBatch (functor trigger)
  quickBatch (traversable trigger)
