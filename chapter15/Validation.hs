module Validation where

import Test.QuickCheck (arbitrary, elements, quickCheck, Arbitrary)
import Assoc

data Validation a b =
  Failure a | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  Failure a <> Failure a' = Failure (a <> a')
  Failure a <> x = x
  x <> _ = x

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Validation a b) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 elements [Failure a, Success b]

type ValidationAssoc =
     Validation String Int
  -> Validation String Int
  -> Validation String Int
  -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: ValidationAssoc)
  let failure :: String
              -> Validation String Int
      failure = Failure
      success :: Int
              -> Validation String Int
      success = Success
  print $ success 1 <> failure "blah"
  print $ failure "woot" <> failure "blah"
  print $ success 1 <> success 2
  print $ failure "woot" <> success 2
