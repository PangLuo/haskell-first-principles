module Pair where

import Test.QuickCheck
import Check

data Pair a = Pair a a
  deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 return $ Pair a b

main :: IO ()
main = do
  quickCheck (functorIdentity :: Pair Int -> Bool)
  quickCheck (functorCompose :: Pair Int
                             -> Fun Int Int
                             -> Fun Int Int
                             -> Bool)
