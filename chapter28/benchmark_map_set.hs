module Main where

import Criterion.Main
import qualified Data.Map as M
import qualified Data.Set as S

bumpIt (i, v) = (i + 1, v + 1)

m :: M.Map Int Int
m = M.fromList $ take 10000 stream
  where stream = iterate bumpIt (0, 0)

s :: S.Set Int
s = S.fromList $ take 10000 stream
  where stream = iterate (+1) 0

insertMap :: Int -> M.Map Int Int
insertMap i = M.insert i i m

insertSet :: Int -> S.Set Int
insertSet i = S.insert i s

main :: IO ()
main = defaultMain
  [ bench "insertion map" $
    whnf insertMap 20000
  , bench "insertion set" $
    whnf insertSet 20000
  ]
