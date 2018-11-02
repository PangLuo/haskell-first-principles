module String where

import Data.List (genericLength, intercalate)

notThe :: String -> Maybe String
notThe s
  | s == "the" = Nothing
  | otherwise = Just s

replaceThe :: String -> String
replaceThe = intercalate " " . go . words
  where go [] = []
        go (w:ws)
          | notThe w == Nothing = "a" : go ws
          | otherwise = w : go ws

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = countTheBeforeVowel' . words
  where countTheBeforeVowel' [] = 0
        countTheBeforeVowel' [_] = 0
        countTheBeforeVowel' ("the":w:ws)
          | head w `elem` "aeiouAEIOU" =  1 + countTheBeforeVowel' ws
        countTheBeforeVowel' (_:xs) = countTheBeforeVowel' xs

countVowels :: String -> Integer
countVowels = genericLength . filter (`elem` "aeiouAEIOU")
