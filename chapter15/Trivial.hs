module Trivial where

import Assoc
import Id
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)

instance Semigroup Trivial where
  Trivial <> Trivial = Trivial

instance Monoid Trivial where
  mempty = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

type TrivAssoc =
  Trivial -> Trivial -> Trivial -> Bool

main :: IO ()
main = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: TrivAssoc)
  quickCheck (mli :: Trivial -> Bool)
  quickCheck (mlr :: Trivial -> Bool)
