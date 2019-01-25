{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (intersperse, concat)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.Socket.ByteString (recv, sendAll)
import Network.Socket hiding (recv)
import System.Environment (getArgs)
import qualified Data.ByteString as BS

main :: IO ()
main = withSocketsDo $ do
  addrinfos <-
    getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
      Nothing (Just "78")
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  connect sock $ addrAddress serveraddr
  args <- getArgs
  let msg = TE.encodeUtf8 $ T.pack $ concat $ intersperse "|" args
  case BS.null msg of
    True -> do
      putStrLn "user data are empty\nusage: client \"username\"\
                \ \"shell\" \"homeDirectory\" \"realName\" \"phone\""
    _ -> do
      sendAll sock msg
      msg <- recv sock 1024
      putStr $ T.unpack $ TE.decodeUtf8 msg
  sClose sock
