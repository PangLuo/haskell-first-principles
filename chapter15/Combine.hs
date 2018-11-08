module Combine where

import Test.Hspec
import Test.QuickCheck

newtype Combine a b =
  Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine $ f <> g

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine mempty

main :: IO ()
main = hspec $ do
  describe "Combine" $ do
    it "semigroupAssoc" $ do
      property $ \(Fun _ f) (Fun _ g) (Fun _ h) x ->
        unCombine (Combine (f :: String -> Ordering) <>
                   Combine g <> Combine h) x ==
        unCombine ((Combine f <> Combine g) <> Combine h) x
    it "monoidLeftIdentity" $ do
      property $ \(Fun _ f) x ->
        unCombine (mempty <> Combine (f :: String -> Ordering)) x ==
        unCombine (Combine f) x
    it "monoidRightIdentity" $ do
      property $ \(Fun _ f) x ->
        unCombine (Combine (f :: String -> Ordering) <> mempty) x ==
        unCombine (Combine f) x
