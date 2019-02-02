-- ReaderT r Maybe and MaybeT (Reader r) are equivalent. They do the
-- same thing.

module ReaderMaybe where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Maybe

core :: Int -> Maybe Int
core n
  | even n = Just n
  | otherwise = Nothing

f :: ReaderT Int Maybe Int
f = ReaderT core

g :: MaybeT (Reader Int) Int
g = MaybeT $ reader core

main :: IO ()
main = do
  print $ runReaderT f 1
  print $ runReader (runMaybeT g) 1
  print $ runReaderT f 2
  print $ runReader (runMaybeT g) 2
