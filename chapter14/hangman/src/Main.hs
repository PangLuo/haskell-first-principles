module Main where

import Data.Char (toLower)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import Hangman

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzle (map toLower word)
  runGame puzzle
