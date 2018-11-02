module MkWord where

import Data.Char (isAlpha)
import Data.List (genericLength)
import String (countVowels)

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiouAEIOU"

countConsonants :: String -> Integer
countConsonants = genericLength . filter (\c ->
  isAlpha c && (not $ c `elem` vowels))

mkWord :: String -> Maybe Word'
mkWord s
  | countVowels s > countConsonants s = Nothing
  | otherwise = Just $ Word' s
