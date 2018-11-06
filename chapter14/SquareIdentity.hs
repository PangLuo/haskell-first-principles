module SquareIdentity where

import Test.QuickCheck

square x = x * x

squareIdentity = square . sqrt

prop_squareIdentity :: Double -> Bool
prop_squareIdentity x = squareIdentity x == x

main :: IO ()
main = do
{-
  The property fails because the type of sqrt is "Floating a => a -> a" and
  the precision of Floating values is limited.
-}
  quickCheck prop_squareIdentity
