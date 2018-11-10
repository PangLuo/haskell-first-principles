module Two where

import Test.QuickCheck
import Check

data Two a b = Two a b
  deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 return $ Two a b

main :: IO ()
main = do
  quickCheck (functorIdentity :: Two String Int -> Bool)
  quickCheck (functorCompose :: Two String Int
                             -> Fun Int Int
                             -> Fun Int Int
                             -> Bool)
