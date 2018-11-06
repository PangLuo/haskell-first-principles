module Main where

import Data.Set (difference, fromList, isSubsetOf, size)
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Hangman

charGen :: Gen Char
charGen = choose ('a', 'z')

wordGen :: Gen String
wordGen = listOf charGen


puzzleGen :: Gen Puzzle
puzzleGen = do
  word <- suchThat wordGen (\xs ->
    length xs `elem` [minWordLength .. maxWordLength - 1])
  letters <- shuffle ['a'..'z']

  let getFails xs = size $ difference (fromList xs) (fromList word)
  guessed <- suchThat (sublistOf letters) (\xs ->
    (not $ isSubsetOf (fromList word) (fromList xs)) &&
    getFails xs < failLimit)

  let fails = getFails guessed
      discovered = map (\c -> if elem c guessed then Just c else Nothing) word
  return (Puzzle word discovered guessed fails)


data PuzzleChar = PuzzleChar Puzzle Char
  deriving Show

puzzleCharGen :: Gen PuzzleChar
puzzleCharGen = do
  p <- puzzleGen
  c <- charGen
  return (PuzzleChar p c)

instance Arbitrary PuzzleChar where
  arbitrary = puzzleCharGen

prop_handleGuess :: PuzzleChar -> Property
prop_handleGuess (PuzzleChar puzzle guess) = monadicIO $ do
  puzzle' <- run $ handleGuess puzzle guess
  case (charInWord puzzle guess,
        alreadyGuessed puzzle guess) of
    (_, True) ->
      assert (puzzle == puzzle')
    (True, _) ->
      assert (
        charInWord puzzle guess &&
        charInWord puzzle' guess &&
        notElem (Just guess) (found puzzle) &&
        elem (Just guess) (found puzzle') &&
        not (alreadyGuessed puzzle guess) &&
        alreadyGuessed puzzle' guess &&
        failures puzzle' == failures puzzle)
    (False, _) ->
      assert (
        not (charInWord puzzle guess) &&
        not (charInWord puzzle' guess) &&
        notElem (Just guess) (found puzzle) &&
        notElem (Just guess) (found puzzle') &&
        not (alreadyGuessed puzzle guess) &&
        alreadyGuessed puzzle' guess &&
        failures puzzle' == failures puzzle + 1)

main :: IO ()
main = do
  quickCheck prop_handleGuess
