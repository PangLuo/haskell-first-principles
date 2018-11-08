module Three where

import Test.QuickCheck
import Assoc

data Three a b c = Three a b c deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) =>
         Semigroup (Three a b c) where
  Three a b c <> Three a' b' c' = Three (a <> a') (b <> b') (c <> c')

instance (Arbitrary a, Arbitrary b, Arbitrary c) =>
         Arbitrary (Three a b c) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 c <- arbitrary
                 return $ Three a b c

type ThreeAssoc =
     Three String Ordering (Either String Int)
  -> Three String Ordering (Either String Int)
  -> Three String Ordering (Either String Int)
  -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: ThreeAssoc)
