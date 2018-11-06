{-|
Modifications compared to the code in the book include
- shorter words
- only incorrect guesses count towards the guess limit:
  * the fourth argument of Puzzle indicates the number of failed attempts
    so far
  * once the number of failed attempts reaches failLimit, you lose
  * a guessed character that has already been guessed before isn't regarded
    as a failed attempt
-}

module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.IO (hSetBuffering, stdout, BufferMode(..))
import System.Random (randomRIO)

type WordList = [String]

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ lines dict

minWordLength :: Int
minWordLength = 3

maxWordLength :: Int
maxWordLength = 6

gameWords :: IO WordList
gameWords = do
  aw <- allWords
  return $ filter gameLength aw
  where gameLength w =
          let l = length w
          in l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord wl = do
  randomIndex <- randomRIO (0, length wl - 1)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

failLimit :: Int
failLimit = 7

data Puzzle =
  Puzzle String [Maybe Char] [Char] Int

instance Show Puzzle where
  show (Puzzle _ discovered guessed fails) =
    (intersperse ' ' $
     map renderPuzzleChar discovered)
    ++ " Guessed so far: " ++ guessed
    ++ " Chances left: " ++ (show $ failLimit - fails)

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) [] 0

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _ _) = flip elem s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ s _) = flip elem s

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar (Just c) = c
renderPuzzleChar _ = '_'

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s fails) c =
  Puzzle word newFilledInSoFar (c:s) newFails
  where zipper guessed wordChar guessChar =
          if wordChar == guessed
            then Just wordChar
            else guessChar
        newFilledInSoFar =
          zipWith (zipper c) word filledInSoFar
        newFails =
          if filledInSoFar == newFilledInSoFar
            then fails + 1
            else fails

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word, filling in the word\
              \ accordingly"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ _ fails) =
  if fails == failLimit then
    do putStrLn "You lose!"
       putStrLn $ "The word was: " ++ wordToGuess
       exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _ _) =
  if all isJust filledInSoFar then
    do putStrLn "You win!"
       exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzle (map toLower word)
  runGame puzzle
