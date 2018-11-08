module OptionalWrapper where

import Test.QuickCheck

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Semigroup (First' a) where
  First' (Only x) <> _ = First' (Only x)
  First' Nada <> x = x

instance Monoid (First' a) where
  mempty = First' Nada

genFirst' :: Arbitrary a => Gen (First' a)
genFirst' = do
  a <- arbitrary
  frequency [(3, return (First' $ Only a)),
             (1, return (First' Nada))]

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = genFirst'

firstMappend :: First' a
             -> First' a
             -> First' a
firstMappend = mappend

type FirstMappend =
     First' String
  -> First' String
  -> First' String
  -> Bool

type FstId = First' String -> Bool

monoidAssoc :: (Eq m, Monoid m)
            => m -> m -> m -> Bool
monoidAssoc a b c =
  (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m)
                   => m
                   -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m)
                    => m
                    -> Bool
monoidRightIdentity a = (a <> mempty) == a

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
