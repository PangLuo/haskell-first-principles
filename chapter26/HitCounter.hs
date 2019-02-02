{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config =
  Config {
    counts :: IORef (M.Map Text Integer)
  , prefix :: Text
  }

type Scotty =
  ScottyT Text (ReaderT Config IO)

bumpBoomp :: Text
          -> M.Map Text Integer
          -> (M.Map Text Integer, Integer)
bumpBoomp k m =
  case M.insertLookupWithKey (\k nv ov -> ov + nv) k 1 m of
    (Just v, m') -> (m', v + 1)
    (_, m') -> (m', 1)

app :: Scotty ()
app =
  get "/:key" $ do
    unprefixed <- param "key"
    prefix' <- lift $ asks prefix
    let key' = mappend prefix' unprefixed
    counts' <- lift $ asks counts
    (newMap, newInteger) <-
      liftIO $ bumpBoomp key' <$> readIORef counts'
    liftIO $ writeIORef counts' newMap
    html $ mconcat [ "<h1>Success! Count was: "
                   , TL.pack $ show newInteger
                   , "</h1>"
                   ]

main :: IO ()
main = do
  [prefixArg] <- getArgs
  counter <- newIORef M.empty
  let config = Config counter $ TL.pack prefixArg
      runR = flip runReaderT config
  scottyT 3000 runR app
