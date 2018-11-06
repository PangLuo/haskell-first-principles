module Person where

import System.IO (hSetBuffering, stdout, BufferMode(..))

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid =
    NameEmpty
  | AgeTooLow
  | PersonInvalidUnknown String
  deriving (Eq, Show)

mkPerson :: Name
         -> Age
         -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 =
      Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise =
      Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++
        " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  hSetBuffering stdout NoBuffering
  putStr "Enter your name: "
  name <- getLine
  putStr "Enter your age: "
  age <- getLine
  let person = mkPerson name (read age)
  case person of
    Right p -> putStrLn $ "Yay! Successfully got a person: " ++ show p
    Left e -> putStrLn $ "Error occurred: " ++ show e
