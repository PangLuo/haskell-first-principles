module Two where

import Test.QuickCheck
import Assoc
import Id

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a b <> Two a' b' = Two (a <> a') (b <> b')

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do a <- arbitrary
                 b <- arbitrary
                 return $ Two a b

type TwoAssoc =
  Two String Ordering -> Two String Ordering -> Two String Ordering -> Bool

main :: IO ()
main = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: TwoAssoc)
  quickCheck (mli :: Two String Ordering -> Bool)
  quickCheck (mlr :: Two String Ordering -> Bool)
