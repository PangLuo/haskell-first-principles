module Identity where

import Test.QuickCheck
import Assoc
import Id

newtype Identity a = Identity a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  Identity a <> Identity a' = Identity $ a <> a'

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do a <- arbitrary
                 return $ Identity a

type IdentityAssoc =
  Identity String -> Identity String -> Identity String -> Bool

main :: IO ()
main = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: IdentityAssoc)
  quickCheck (mli :: Identity String -> Bool)
  quickCheck (mlr :: Identity String -> Bool)
