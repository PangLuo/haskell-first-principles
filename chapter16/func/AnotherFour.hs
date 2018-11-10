module AnotherFour where

import Test.QuickCheck
import Check

data Four' a b = Four' a a a b
  deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Four' a b) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 c <- arbitrary
                 d <- arbitrary
                 return $ Four' a b c d

main :: IO ()
main = do
  quickCheck (functorIdentity :: Four' String Int -> Bool)
  quickCheck (functorCompose :: Four' String Int
                             -> Fun Int Int
                             -> Fun Int Int
                             -> Bool)
