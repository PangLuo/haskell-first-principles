module ZipList where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
    Nil
  | Cons a (List a)
  deriving (Eq, Show)

take' :: Int -> List a -> List a
take' _ Nil = Nil
take' n (Cons a as)
  | n <= 0 = Nil
  | otherwise = Cons a $ take' (n - 1) as

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a as) = Cons (f a) $ fmap f as

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys =
  Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' (fmap f as)

instance Applicative List where
  pure a = Cons a Nil
  (<*>) fs as = flatMap (flip fmap as) fs

instance Eq a => EqProp (List a) where
  (=-=) = eq

listGen :: Arbitrary a => Gen (List a)
listGen = do
  x <- arbitrary
  xs <- arbitrary
  elements [Cons x xs, Nil]

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = listGen

trigger :: List (String, Int, Int)
trigger = undefined

newtype ZipList' a =
  ZipList' (List a)
  deriving (Eq, Show)

instance Functor ZipList' where
  fmap f (ZipList' xs) =
    ZipList' $ fmap f xs

repeat' :: a -> List a
repeat' a = Cons a $ repeat' a

zip' :: List a -> List b -> List (a, b)
zip' Nil _ = Nil
zip' _ Nil = Nil
zip' (Cons a as) (Cons b bs) = Cons (a, b) $ zip' as bs

instance Applicative ZipList' where
  pure a = ZipList' $ repeat' a
  ZipList' fs <*> ZipList' as =
    ZipList' $ fmap (\(f, a) -> f a) $ zip' fs as

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

trigger' :: ZipList' (String, String, Int)
trigger' = undefined

main :: IO ()
main = do
  putStrLn "List Check"
  quickBatch (functor trigger)
  quickBatch (applicative trigger)

  putStrLn "ZipList Check"
  quickBatch (functor trigger')
  quickBatch (applicative trigger')
