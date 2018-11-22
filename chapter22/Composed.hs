module Composed where

import Control.Applicative
import Data.Char

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = liftA2 (,) cap rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = (,) <$> rev <*> cap

tupledDo :: [Char] -> ([Char], [Char])
tupledDo = do
  a <- cap
  b <- rev
  return (a, b)

tupledBind :: [Char] -> ([Char], [Char])
tupledBind =
  cap >>=
  \a ->
    rev >>=
      \b -> return (a, b)
