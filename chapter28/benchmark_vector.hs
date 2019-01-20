module Main where

-- stack ghc -- -prof -fprof-auto -rtsopts -O2 benchmark_vector.hs
-- ./benchmark_vector +RTS -P > result
-- cat benchmark_vector.prof
-- note %alloc of v and v

import Control.Monad
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV

xs :: [Int]
xs = [1..100000]

v :: V.Vector Int
v = V.fromList xs

uv :: UV.Vector Int
uv = UV.fromList xs

main :: IO ()
main = do
  print v
  print uv
