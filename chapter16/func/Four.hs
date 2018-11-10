module Four where

import Test.QuickCheck
import Check

data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 c <- arbitrary
                 d <- arbitrary
                 return $ Four a b c d

main :: IO ()
main = do
  quickCheck (functorIdentity :: Four String String Int Int -> Bool)
  quickCheck (functorCompose :: Four String String Int Int
                             -> Fun Int Int
                             -> Fun Int Int
                             -> Bool)
