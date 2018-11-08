module Or where

import Test.QuickCheck
import Assoc

data Or a b =
    Fst a
  | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  Snd a <> _ = Snd a
  _ <> x = x

instance (Arbitrary a, Arbitrary b) =>
         Arbitrary (Or a b) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 elements [Fst a, Snd b]

type OrAssoc =
     Or String Ordering
  -> Or String Ordering
  -> Or String Ordering
  -> Bool

main :: IO ()
main =
  quickCheck (semigroupAssoc :: OrAssoc)
