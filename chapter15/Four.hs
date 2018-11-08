module Four where

import Test.QuickCheck
import Assoc

data Four a b c d = Four a b c d deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) =>
         Semigroup (Four a b c d) where
  Four a b c d <> Four a' b' c' d' = Four (a <> a') (b <> b') (c <> c') (d <> d')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) =>
         Arbitrary (Four a b c d) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 c <- arbitrary
                 d <- arbitrary
                 return $ Four a b c d

type FourAssoc =
     Four String Ordering String Ordering
  -> Four String Ordering String Ordering
  -> Four String Ordering String Ordering
  -> Bool

main :: IO ()
main =
  quickCheck (semigroupAssoc :: FourAssoc)
