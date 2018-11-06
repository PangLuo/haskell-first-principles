module Ciphers where

import Data.Char
import System.IO (hSetBuffering, stdout, BufferMode(..))

caesar :: Int -> String -> String
caesar _ [] = []
caesar n (x:xs)
  | isAlpha x = (chr $ ((ord x + n - base) `mod` 26 + base)) : caesar n xs
  | otherwise = x : caesar n xs
    where base = if isUpper x then ord 'A' else ord 'a'

unCaesar :: Int -> String -> String
unCaesar n = caesar $ negate n

type Key = String

vigenere' :: Bool -> [(Char, Char)] -> [Char]
vigenere' _ [] = []
vigenere' b ((c, k) : xs)
  | isAlpha c =
      (chr $ ((ord c + dist - base) `mod` 26 + base)) : vigenere' b xs
  | otherwise = c : vigenere' b xs
    where base = if isUpper c then ord 'A' else ord 'a'
          dist = if b then d else -d
                 where d = ord k - ord (if isUpper k then 'A' else 'a')

newKey :: String -> Key -> [(Char, Char)]
newKey [] _ = []
newKey _ [] = error "no key provided"
newKey (x:xs) (k:ks)
  | isAlpha x = (x, k) : newKey xs ks
  | otherwise = (x, x) : newKey xs (k:ks)

vigenere :: Key -> String -> String
vigenere "" s = s
vigenere k s = vigenere' True $ newKey s $ concat $ repeat k

unVigenere :: Key -> String -> String
unVigenere "" s = s
unVigenere k s = vigenere' False $ newKey s $ concat $ repeat k

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStr "Enter a string you want to encode: "
  str <- getLine
  putStr "Enter the number of shifts for the Caesar cipher: "
  shiftStr <- getLine
  let shift = (read shiftStr :: Int)
      encoded = caesar shift str
  putStrLn $ "Caesar encoded: " ++ encoded
  putStrLn $ "Caesar decoded: " ++ unCaesar shift encoded

  putStr "Enter the keyword for the Vigenère cipher: "
  key <- getLine
  let encoded = vigenere key str
  putStrLn $ "Vigenère encoded: " ++ encoded
  putStrLn $ "Vigenère decoded: " ++ unVigenere key encoded
