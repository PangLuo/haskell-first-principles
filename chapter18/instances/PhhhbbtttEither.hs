module PhhhbbtttEither where

import Prelude hiding (Left, Right)

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data PhhhbbtttEither b a =
    Left a
  | Right b
  deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Left a) = Left (f a)
  fmap _ (Right b) = Right b

instance (Monoid b) => Applicative (PhhhbbtttEither b) where
  pure = Left
  Right e <*> _ = Right e
  Left f <*> r = fmap f r

instance (Monoid b) => Monad (PhhhbbtttEither b) where
  return = pure
  Right e >>= _ = Right e
  Left r >>= k = k r

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where
  (=-=) = eq

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 elements [Left a, Right b]

trigger :: PhhhbbtttEither String (String, Ordering, Int)
trigger = undefined

main :: IO ()
main = do
  quickBatch $ functor trigger
  quickBatch $ applicative trigger
  quickBatch $ monad trigger
