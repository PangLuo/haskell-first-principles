module QuickCheckHspec where

import Data.List (sort)
import Test.Hspec
import Test.QuickCheck

half x = x / 2
halfIdentity = (*2) . half

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
  where go _ status@(_, False) = status
        go y (Nothing, t) = (Just y, t)
        go y (Just x, t) = (Just y, x >= y)

plusAssociative x y z =
  x + (y + z) == (x + y) + z
plusCommutative x y =
  x + y == y + x

multAssociative x y z =
  x * (y * z) == (x * y) * z
multCommutative x y =
  x * y == y * x

remQuot x y =
  (quot x y) * y + (rem x y) == x

modDiv x y =
  (div x y) * y + (mod x y) == x

powAssociative x y z =
  x ^ (y ^ z) == (x ^ y) ^ z
powCommutative x y =
  x ^ y == y ^ x

main :: IO ()
main = hspec $ do
  describe "QuickCheck Tests" $ do
    it "halfIdentity" $ do
      property $ \x -> halfIdentity (x :: Rational) == x

    it "listOrdered" $ do
      property $ \xs -> (listOrdered $ sort (xs :: [Int])) == True

    it "plusAssociative" $ do
      property $ \x y z -> plusAssociative (x :: Integer) y z == True

    it "plusCommutative" $ do
      property $ \x y -> plusCommutative (x :: Integer) y == True

    it "multAssociative" $ do
      property $ \x y z -> multAssociative (x :: Integer) y z == True

    it "multCommutative" $ do
      property $ \x y -> multCommutative (x :: Integer) y == True

    it "remQuot" $ do
      property $ \x y -> y /= 0 ==> remQuot (x :: Integer) y == True

    it "modDiv" $ do
      property $ \x y -> y /= 0 ==> modDiv (x :: Integer) y == True

    it "powAssociative" $ do
      property $ \x y z ->
        powAssociative (x :: Integer) (y :: Integer) (z :: Integer) == True

    it "powCommutative" $ do
      property $ \x y ->
        powCommutative (x :: Integer) (y :: Integer) == True

    it "reversing a list twice is the same as the identity of the list" $ do
      property $ \xs -> (reverse . reverse) (xs :: [Int]) == id xs

    it "definition of $" $ do
      property $ \(Fun _ f) a -> ((f :: Int -> Int) $ a) == f a

    it "definition of ." $ do
      property $ \(Fun _ f) (Fun _ g) x ->
        ((f :: Int -> Int) . g) (x :: Int) == f (g x)

    it "foldr (:) == (++)" $ do
      property $ \xs ys -> foldr (:) (xs :: [Int]) (ys :: [Int]) == (++) xs ys

    it "foldr (++) [] == concat" $ do
      property $ \xs -> foldr (++) [] (xs :: [[Int]]) == concat xs

    it "length (take n xs) == n" $ do
      property $ \n xs -> length (take n (xs :: [Int])) == n

    it "f x = (read (show x)) == x" $ do
      property $ \x -> read (show x) == (x :: Int)
