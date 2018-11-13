module AnotherThree where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  Three' x f g <*> Three' y a b = Three' (x <> y) (f a) (g b)

instance (Eq a, Eq b) => EqProp (Three' a b) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Three' a b) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 c <- arbitrary
                 return $ Three' a b c

trigger :: Three' String (String, String, Int)
trigger = undefined

main :: IO ()
main = do
  quickBatch (functor trigger)
  quickBatch (applicative trigger)
