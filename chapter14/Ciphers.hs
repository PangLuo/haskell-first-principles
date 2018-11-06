module Ciphers where

import Data.Char
import System.IO (hSetBuffering, stdout, BufferMode(..))
import Test.QuickCheck

caesar :: Int -> String -> String
caesar _ [] = []
caesar n (x:xs)
  | isAlpha x && isAscii x =
      (chr $ ((ord x + n - base) `mod` 26 + base)) : caesar n xs
  | otherwise = x : caesar n xs
    where base = if isUpper x then ord 'A' else ord 'a'

unCaesar :: Int -> String -> String
unCaesar n = caesar $ negate n

type Key = String

vigenere' :: Bool -> [(Char, Char)] -> [Char]
vigenere' _ [] = []
vigenere' b ((c, k) : xs)
  | isAlpha c && isAscii c =
      (chr $ ((ord c + dist - base) `mod` 26 + base)) : vigenere' b xs
  | otherwise = c : vigenere' b xs
    where base = if isUpper c then ord 'A' else ord 'a'
          dist = if b then d else -d
                 where d = ord k - ord (if isUpper k then 'A' else 'a')

newKey :: String -> Key -> [(Char, Char)]
newKey [] _ = []
newKey _ [] = error "no key provided"
newKey (x:xs) (k:ks)
  | isAlpha x && isAscii x = (x, k) : newKey xs ks
  | otherwise = (x, x) : newKey xs (k:ks)

vigenere :: Key -> String -> String
vigenere "" s = s
vigenere k s = vigenere' True $ newKey s $ concat $ repeat k

unVigenere :: Key -> String -> String
unVigenere "" s = s
unVigenere k s = vigenere' False $ newKey s $ concat $ repeat k

prop_caesar :: Int -> String -> Bool
prop_caesar n s = unCaesar n (caesar n s) == s

prop_vigenere :: String -> Property
prop_vigenere s =
  forAll (listOf $ choose ('a', 'z'))
    (\k -> unVigenere k (vigenere k s) == s)

main :: IO ()
main = do
  quickCheck prop_caesar
  quickCheck prop_vigenere
