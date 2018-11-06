module Addition where

import Test.Hspec

myMul' :: Integral a => a -> a -> a
myMul' x y = go x y
  where go x' count
          | count == 0 = 0
          | otherwise = x' + go x' (count - 1)

myMul :: Integral a => a -> a -> a
myMul x y = (signum x) * (signum y) * myMul' (abs x) (abs y)

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
    it "15 multiplied by 3 is 45" $ do
      myMul 15 3 `shouldBe` (45 :: Integer)
    it "15 multiplied by -3 is -45" $ do
      myMul 15 (-3) `shouldBe` (-45 :: Integer)
    it "-15 multiplied by 3 is -45" $ do
      myMul (-15) 3 `shouldBe` (-45 :: Integer)
    it "-15 multiplied by -3 is 45" $ do
      myMul (-15) (-3) `shouldBe` (45 :: Integer)
