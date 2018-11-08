module BoolDisj where

import Test.QuickCheck
import Assoc
import Id

newtype BoolDisj = BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup BoolDisj where
  BoolDisj True <> _ = BoolDisj True
  _ <> a = a

instance Monoid BoolDisj where
  mempty = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = do a <- arbitrary
                 return $ BoolDisj a

type BoolDisjAssoc =
  BoolDisj -> BoolDisj -> BoolDisj -> Bool

main :: IO ()
main = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: BoolDisjAssoc)
  quickCheck (mli :: BoolDisj -> Bool)
  quickCheck (mlr :: BoolDisj -> Bool)
