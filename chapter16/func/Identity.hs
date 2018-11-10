module Identity where

import Test.QuickCheck
import Check

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do a <- arbitrary
                 return $ Identity a

main :: IO ()
main = do
  quickCheck (functorIdentity :: Identity Int -> Bool)
  quickCheck (functorCompose :: Identity Int
                             -> Fun Int Int
                             -> Fun Int Int
                             -> Bool)
