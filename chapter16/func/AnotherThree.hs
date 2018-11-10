module AnotherThree where

import Test.QuickCheck
import Check

data Three' a b = Three' a b b
  deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Three' a b) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 c <- arbitrary
                 return $ Three' a b c

main :: IO ()
main = do
  quickCheck (functorIdentity :: Three' String Int -> Bool)
  quickCheck (functorCompose :: Three' String Int
                             -> Fun Int Int
                             -> Fun Int Int
                             -> Bool)
