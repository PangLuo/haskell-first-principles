module Main where

import Control.Monad (replicateM_)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import System.Random (randomRIO)
import System.IO (hFlush, stdout)

rounds :: Int
rounds = 10

lines :: Int
lines = 50

play :: StateT (Int, Int) IO ()
play = do
  lift $ putStr "P1: "
  lift $ hFlush stdout
  i <- lift $ (read :: String -> Int) <$> getLine
  lift $ replicateM_ 50 $ putStrLn ""
  lift $ putStr "P2: "
  lift $ hFlush stdout
  j <- lift $ (read :: String -> Int) <$> getLine
  lift $ replicateM_ 50 $ putStrLn ""
  case odd (i + j) of
    True -> do
      modify $ \(p1, p2) -> (p1 + 1, p2)
      lift $ putStrLn "- P1 wins\n"
    _ -> do
      modify $ \(p1, p2) -> (p1, p2 + 1)
      lift $ putStrLn "- P2 wins\n"

main :: IO ()
main = do
  putStrLn
    "-- P1 is Player 1\n\
    \-- P2 is Player 2\n\
    \-- P1 is odds. P2 is evens.\n\
    \-- The game consists of 10 rounds. For each round, both\n\
    \   participants input 1 or 2. If the sum of the two numbers\n\
    \   is odd, Player 1 wins, otherwise Player 2 is the winner\n\
    \   for this round.\n"
  (p1, p2) <-
    execStateT (replicateM_ rounds play) (0, 0)
  putStrLn $ "Player 1's score: " ++ show p1 ++
    "\nPlayer 2's score: " ++ show p2
  if p1 > p2
    then putStrLn "Player 1 wins. Congratulations!"
    else
      if p1 < p2
        then putStrLn "Player 2 wins. Congratulations!"
        else putStrLn "Aha. It's a draw."
