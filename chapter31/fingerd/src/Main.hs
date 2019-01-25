{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import Control.Concurrent (forkIO)
import Control.Exception
import Control.Monad (forever)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Typeable
import Database.SQLite.Simple hiding (close)
import qualified Database.SQLite.Simple as SQLite
import Database.SQLite.Simple.Types
import Network.Socket hiding (close, recv)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Socket.ByteString (recv, sendAll)
import Text.RawString.QQ

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

instance ToRow User where
  toRow (User id_ username shell homeDir realName phone) =
    toRow (id_, username, shell, homeDir, realName, phone)

createUsers :: Query
createUsers = [r|
CREATE TABLE IF NOT EXISTS users
  (id INTEGER PRIMARY KEY AUTOINCREMENT,
   username TEXT UNIQUE,
   shell TEXT, homeDirectory TEXT,
   realName TEXT, phone TEXT)
|]

insertUser :: Query
insertUser =
  "INSERT INTO users\
  \ VALUES (?, ?, ?, ?, ?, ?)"

allUsers :: Query
allUsers =
  "SELECT * from users"

getUserQuery :: Query
getUserQuery =
  "SELECT * from users where username = ?"

data DuplicateData = DuplicateData
  deriving (Eq, Show, Typeable)

instance Exception DuplicateData

type UserRow =
  (Null, Text, Text, Text, Text, Text)

type Routine = Connection -> Socket -> IO ()

db :: String
db = "finger.db"

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

createDatabase :: IO ()
createDatabase = do
  conn <- open db
  execute_ conn createUsers
  execute conn insertUser meRow
  rows <- query_ conn allUsers
  mapM_ print (rows :: [User])
  SQLite.close conn
  where meRow :: UserRow
        meRow =
          (Null, "callen", "/bin/zsh",
           "/home/callen", "Chris Allen",
           "555-123-4567")

returnUsers :: Routine
returnUsers dbConn soc = do
  rows <- query_ dbConn allUsers
  let usernames = map username rows
      newlineSeparated =
        T.concat $ intersperse "\n" usernames
  sendAll soc (encodeUtf8 newlineSeparated)

formatUser :: User -> ByteString
formatUser (User _ username shell
            homeDir realName _) = BS.concat
  ["Login: ", e username, "\t\t\t\t",
   "Name: ", e realName, "\n",
   "Directory: ", e homeDir, "\t\t\t",
   "Shell: ", e shell, "\n"]
  where e = encodeUtf8

returnUser :: Connection
           -> Socket
           -> Text
           -> IO ()
returnUser dbConn soc username = do
  maybeUser <-
    getUser dbConn (T.strip username)
  case maybeUser of
    Nothing -> do
      putStrLn
        ("Couldn't find matching user\
          \ for username: "
          ++ (show username))
    Just user ->
      sendAll soc (formatUser user)

handleQuery :: Routine
handleQuery dbConn soc = do
  msg <- recv soc 1024
  case msg of
    "\r\n" -> returnUsers dbConn soc
    name ->
      returnUser dbConn soc (decodeUtf8 name)

handleQueries :: Routine
handleQueries dbConn sock = forever $ do
  (soc, _) <- accept sock
  putStrLn "Got data connection, handling query"
  handleQuery dbConn soc
  sClose soc

handleInsertion :: Routine
handleInsertion dbConn sock = forever $ do
  (soc, _) <- accept sock
  putStrLn "Got control connection, handling insertion"
  msg <- recv soc 1024
  let cmd = T.splitOn "|" (decodeUtf8 msg)
  case cmd of
    [username', shell', homeDirectory', realName', phone'] -> do
      let username'' = T.strip username'
      case T.null username'' of
        True -> sendAll soc "username must not be empty\n"
        _ -> do
          maybeUser <- getUser dbConn username''
          case maybeUser of
            Just _ -> sendAll soc "username exists\n"
            Nothing -> do
              execute dbConn insertUser (Null, username'',
                shell', homeDirectory', realName', phone')
              sendAll soc "user added\n"
    _ -> sendAll soc "incorrect format\n"
  sClose soc

serve :: ServiceName -> Routine -> IO ()
serve port routine = withSocketsDo $ do
  addrinfos <-
    getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
      Nothing (Just port)
  let serveraddr = head addrinfos
  sock <- socket (addrFamily serveraddr) Stream defaultProtocol
  bindSocket sock (addrAddress serveraddr)
  listen sock 1
  conn <- open db
  routine conn sock
  SQLite.close conn
  sClose sock

main :: IO ()
main = do
  -- in order to add a user, client can send msg in the form of
  -- "username|shell|homeDirectory|realName|phone" to server
  -- via port 78
  forkIO $ serve "78" handleInsertion
  serve "79" handleQueries
