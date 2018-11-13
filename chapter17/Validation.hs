module Validation where

import Test.QuickCheck (Arbitrary, arbitrary, elements, quickCheck)
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a =
    Failure e
  | Success a
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success a) = Success (f a)

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

instance (Arbitrary e, Arbitrary a) =>
         Arbitrary (Validation e a) where
  arbitrary = do e <- arbitrary
                 a <- arbitrary
                 elements [Failure e, Success a]

instance Monoid e =>
         Applicative (Validation e) where
  pure = Success
  Failure e <*> Failure e' = Failure $ e <> e'
  Failure e <*> _ = Failure e
  Success f <*> r = fmap f r

trigger :: Validation String (String, String, Int)
trigger = undefined

validToEither :: Validation e a -> Either e a
validToEither (Failure err) = Left err
validToEither (Success a) = Right a

eitherToValid :: Either e a -> Validation e a
eitherToValid (Left err) = Failure err
eitherToValid (Right a) = Success a

prop_valid :: Validation String Int -> Bool
prop_valid x = (eitherToValid . validToEither) x == x

prop_either :: Either String Int -> Bool
prop_either x = (validToEither . eitherToValid) x == x

main = do
  quickBatch (functor trigger)
  quickBatch (applicative trigger)

  putStrLn "\ntransformation tests:"
  quickCheck prop_valid
  quickCheck prop_either
