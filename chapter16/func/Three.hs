module Three where

import Test.QuickCheck
import Check

data Three a b c = Three a b c
  deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 c <- arbitrary
                 return $ Three a b c

main :: IO ()
main = do
  quickCheck (functorIdentity :: Three String Int Int -> Bool)
  quickCheck (functorCompose :: Three String Int Int
                             -> Fun Int Int
                             -> Fun Int Int
                             -> Bool)
