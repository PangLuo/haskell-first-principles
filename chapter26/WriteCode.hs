module WriteCode where

import Control.Monad.Identity
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

rDec :: Num a => Reader a a
rDec = reader $ subtract 1

rShow :: Show a
      => ReaderT a Identity String
rShow = ReaderT $ Identity <$> show

rPrintAndInc :: (Num a, Show a)
             => ReaderT a IO a
rPrintAndInc = ReaderT $
  \a -> putStrLn ("Hi: " ++ show a) >> return (a + 1)

sPrintIncAccum :: (Num a, Show a)
               => StateT a IO String
sPrintIncAccum = StateT $
  \s ->
    let ss = show s
    in putStrLn ("Hi: " ++ ss) >> return (ss, s + 1)
