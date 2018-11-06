module Palindrome where

import Control.Monad
import Data.Char (isAlphaNum, toLower)
import System.Exit (exitSuccess)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (line1 == reverse line1) of
    True -> putStrLn "It's a palindrome!"
    False -> do putStrLn "Nope!"
                exitSuccess

palindrome' :: IO ()
palindrome' = forever $ do
  line1 <- getLine
  let line1' = map toLower . filter isAlphaNum $ line1
  case (line1' == reverse line1') of
    True -> putStrLn "It's a palindrome!"
    False -> do putStrLn "Nope!"
                exitSuccess
