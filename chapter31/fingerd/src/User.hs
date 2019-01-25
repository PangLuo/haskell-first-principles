{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception
import Control.Monad
import Data.Text (Text, pack, strip)
import qualified Data.Text as T
import Database.SQLite.Simple
import Database.SQLite.Simple.Types
import System.Exit
import Data.Typeable

insertUser :: Query
insertUser =
  "INSERT INTO users\
  \ VALUES (?, ?, ?, ?, ?, ?)"

getUserQuery :: Query
getUserQuery =
  "SELECT * from users where username = ?"

updateUser :: Query
updateUser =
  "UPDATE users SET shell = ?, homeDirectory = ?, realName = ?,\
   \ phone = ? where username = ?"

type UserRow =
  (Null, Text, Text, Text, Text, Text)

data User =
  User {
      userId :: Integer
    , username :: Text
    , shell :: Text
    , homeDirectory :: Text
    , realName :: Text
    , phone :: Text
  } deriving (Eq, Show)

instance FromRow User where
  fromRow = User <$> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field
                 <*> field

db :: String
db = "finger.db"

data DuplicateData = DuplicateData
  deriving (Eq, Show, Typeable)

instance Exception DuplicateData

getUser :: Connection
        -> Text
        -> IO (Maybe User)
getUser conn username = do
  results <-
    query conn getUserQuery (Only username)
  case results of
    [] -> return Nothing
    [user] -> return $ Just user
    _ -> throwIO DuplicateData

addUser :: IO ()
addUser = do
  putStrLn "username:"
  username' <- (strip . pack) <$> getLine
  case T.null username' of
    True -> do
      putStrLn "username must not be empty"
    _ -> do
      conn <- open db
      maybeUser <- getUser conn username'
      case maybeUser of
        Just _ -> do
          putStrLn "username exists."
        Nothing -> do
          putStrLn "shell:"
          shell' <- pack <$> getLine
          putStrLn "homeDirectory:"
          homeDirectory' <- pack <$> getLine
          putStrLn "realName:"
          realName' <- pack <$> getLine
          putStrLn "phone:"
          phone' <- pack <$> getLine
          let row :: UserRow
              row = (Null, username', shell', homeDirectory',
                     realName', phone')
          execute conn insertUser row
      close conn

update :: Text -> String -> IO Text
update t s = do
  putStrLn $ "current " ++ s ++ ":"
  putStrLn $ T.unpack t
  putStrLn $ "new " ++ s ++ ":"
  t' <- fmap pack getLine
  case T.null t' of
    True -> return t
    _ -> return t'

modifyUser :: IO ()
modifyUser = do
  putStrLn "username:"
  username' <- (strip . pack) <$> getLine
  conn <- open db
  maybeUser <-
    getUser conn username'
  case maybeUser of
    Nothing -> do
      putStrLn
        ("Couldn't find matching user\
          \ for username: "
          ++ (show username'))
    Just (User _ username' shell'
          homeDirectory' realName' phone') -> do
      putStrLn ("update information (to keep a field untouched,\
                 \ just press enter)")
      shell'' <- update shell' "shell"
      homeDirectory'' <- update homeDirectory' "homeDirectory"
      realName'' <- update realName' "realName"
      phone'' <- update phone' "phone"
      execute conn updateUser (shell'', homeDirectory'',
        realName'', phone'', username')
  close conn

main :: IO ()
main = forever $ do
  putStrLn "1. add a user\n2. modify a user\n3. quit"
  option <- getLine
  case option of
    "1" -> addUser
    "2" -> modifyUser
    "3" -> exitSuccess
    _ -> putStrLn "incorrect choice"
  putStrLn ""
