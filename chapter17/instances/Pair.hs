module Pair where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Applicative Pair where
  pure a = Pair a a
  Pair f g <*> Pair a b = Pair (f a) (g b)

instance Eq a => EqProp (Pair a) where
  (=-=) = eq

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 return $ Pair a b

trigger :: Pair (String, String, Int)
trigger = undefined

main :: IO ()
main = do
  quickBatch (functor trigger)
  quickBatch (applicative trigger)
