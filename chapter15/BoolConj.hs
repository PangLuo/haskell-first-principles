module BoolConj where

import Test.QuickCheck
import Assoc
import Id

newtype BoolConj = BoolConj Bool
  deriving (Eq, Show)

instance Semigroup BoolConj where
  BoolConj False <> _ = BoolConj False
  _ <> a = a

instance Monoid BoolConj where
  mempty = BoolConj True

instance Arbitrary BoolConj where
  arbitrary = do a <- arbitrary
                 return $ BoolConj a

type BoolConjAssoc =
  BoolConj -> BoolConj -> BoolConj -> Bool

main :: IO ()
main = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: BoolConjAssoc)
  quickCheck (mli :: BoolConj -> Bool)
  quickCheck (mlr :: BoolConj -> Bool)
