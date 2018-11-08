module Comp where

import Test.Hspec
import Test.QuickCheck

newtype Comp a =
  Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
  Comp f <> Comp g = Comp $ f <> g

instance Monoid a => Monoid (Comp a) where
  mempty = Comp $ mempty

main :: IO ()
main = hspec $ do
  describe "Comp" $ do
    it "semigroupAssoc" $ do
      property $ \(Fun _ f) (Fun _ g) (Fun _ h) x ->
        unComp (Comp (f :: String -> String) <>
                Comp g <> Comp h) x ==
        unComp ((Comp f <> Comp g) <> Comp h) x
    it "monoidLeftIdentity" $ do
      property $ \(Fun _ f) x ->
        unComp (mempty <> Comp (f :: String -> String)) x ==
        unComp (Comp f) x
    it "monoidRightIdentity" $ do
      property $ \(Fun _ f) x ->
        unComp (Comp (f :: String -> String) <> mempty) x ==
        unComp (Comp f) x
