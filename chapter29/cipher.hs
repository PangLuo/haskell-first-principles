module Main where

import Data.Char
import System.Environment (getArgs)
import System.IO (hPutStr, stdin, stderr, stdout, hWaitForInput)
import System.Exit (exitFailure)

type Key = String

encode' :: Bool -> [(Char, Char)] -> [Char]
encode' _ [] = []
encode' b ((c, k) : xs)
  | isAlpha c = (chr $ ((ord c + dist - base) `mod` 26 + base))
                  : encode' b xs
  | otherwise = c : encode' b xs
    where base = if isUpper c then ord 'A' else ord 'a'
          dist = if b then d else -d
                 where d = ord k - ord
                             (if isUpper k then 'A' else 'a')

newKey :: String -> Key -> [(Char, Char)]
newKey [] _ = []
newKey (_:_) [] = error "no key provided"
newKey (x:xs) (k:ks)
  | isAlpha x = (x, k) : newKey xs ks
  | otherwise = (x, x) : newKey xs (k:ks)

encode :: String -> Key -> String
encode s k = encode' True $ newKey s $ concat $ repeat k

decode :: String -> Key -> String
decode s k = encode' False $ newKey s $ concat $ repeat k

msg :: String
msg = "incorrent command\nusage: cipher (-d|-e) -k key\n"

timeout :: IO ()
timeout = do
  flg <- hWaitForInput stdin 10000
  if flg
    then return ()
    else do
      hPutStr stderr "no input\n"
      exitFailure

main = do
  args <- getArgs
  case args of
    [m, k, key] ->
      if notElem m ["-d", "-e"] || k /= "-k"
        then do
          hPutStr stderr msg
          exitFailure
        else
          if m == "-d"
            then do
              putStrLn "enter encrypted text:"
              timeout
              txt <- getLine
              putStrLn $
                "decrypted text:\n" ++ decode txt key
            else do
              putStrLn "enter original text:"
              timeout
              txt <- getLine
              putStrLn $
                "encrypted text:\n" ++ encode txt key
    _ -> do
      hPutStr stderr msg
      exitFailure
