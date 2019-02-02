module Main where

import Control.Monad (replicateM_)
import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Sequence ((!?), (<|))
import qualified Data.Sequence as S
import System.Random (randomRIO)
import System.IO (hFlush, stdout)

type Mem = M.Map (Int, Int) Int

rounds :: Int
rounds = 10

play :: StateT (Int, Int, S.Seq Int, Mem) IO ()
play = do
  lift $ putStr "P: "
  lift $ hFlush stdout
  i <- lift $ (read :: String -> Int) <$> getLine
  (p, c, seq, mem) <- get
  tmp <- lift $ randomRIO (1, 2)
  let j = if null mem
            then tmp
            else
              fromMaybe tmp $ M.lookup
                (fromJust (seq !? 1), fromJust (seq !? 0)) mem
      newSeq = i <| seq
      newMem =
        case newSeq !? 2 of
          Just x ->
            M.insert (x, fromJust (newSeq !? 1))
              (fromJust (newSeq !? 0)) mem
          _ -> mem
  lift $ putStrLn $ "C: " ++ show j
  case odd (i + j) of
    True -> do
      put (p + 1, c, newSeq, newMem)
      lift $ putStrLn "- P wins\n"
    _ -> do
      put (p, c + 1, newSeq, newMem)
      lift $ putStrLn "- C wins\n"

main :: IO ()
main = do
  putStrLn
    "-- P is Player\n\
    \-- C is Computer\n\
    \-- Player is odds. Computer is evens.\n"
  (p, c, _, _) <-
    execStateT (replicateM_ rounds play) (0, 0, S.empty, M.empty)
  putStrLn $ "Player's score: " ++ show p ++
    "\nComputer's score: " ++ show c
  if p > c
    then putStrLn "Player wins. Congratulations!"
    else
      if p < c
        then putStrLn "Computer wins. Don't panic."
        else putStrLn "Aha. It's a draw."
