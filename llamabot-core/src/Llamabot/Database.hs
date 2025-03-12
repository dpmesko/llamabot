{-# LANGUAGE OverloadedStrings  #-}


module Llamabot.Database (
  DBUser(..),
  DBChannel(..),
  DBMessage(..),
  DBMetadata(..),
  dbConnect,
  dbClose,
  initializeDB,
  insertUser,
  insertChannel,
  insertMessage,
  insertMetadata,
  selectUsers,
  selectUserById,
  selectUsersByIds,
  selectChannels,
  selectMessages,
  selectMetadata
  ) where


import           Control.Monad

import qualified Data.Map                             as M
import           Data.Maybe
import qualified Data.List                            as L
import           Data.Text
import           Data.Time

import           Database.MySQL.Simple
import           Database.MySQL.Simple.Param
import           Database.MySQL.Simple.Result
import           Database.MySQL.Simple.QueryParams
import           Database.MySQL.Simple.QueryResults



data DBUser = DBUser 
  { userId                 :: Text
  , llamasSentToday        :: Integer
  , llamasReceivedThisWeek :: Integer
  , llamasSentThisWeek     :: Integer
  , llamasReceived         :: Integer
  , llamasSent             :: Integer
  } deriving (Eq, Show)

data DBChannel = DBChannel
  { channelId   :: Text
  , isLogging   :: Bool
  , isListening :: Bool
  } deriving (Eq, Show)

data DBMessage = DBMessage
  { body       :: Text
  , sender     :: Text
  , recipient  :: Text
  , mChannelId :: Text
  , date       :: Day
  } deriving (Eq, Show)


data DBMetadata = DBMetadata { mdCurrentDay :: Day} deriving (Eq, Show)

instance QueryParams DBUser where
  renderParams user = [a, b, c, d, e, f]
    where a = render $ userId user
          b = render $ llamasSentToday user
          c = render $ llamasReceivedThisWeek user
          d = render $ llamasSentThisWeek user
          e = render $ llamasReceived user
          f = render $ llamasSent user

instance QueryResults DBUser where
  convertResults [fa, fb, fc, fd, fe, ff] [va, vb, vc, vd, ve, vf] = DBUser a b c d e f
    where a = convert fa va
          b = convert fb vb
          c = convert fc vc
          d = convert fd vd
          e = convert fe ve
          f = convert ff vf
  convertResults fs vs = convertError fs vs 6


instance QueryParams DBChannel where
   renderParams channel = [a, b, c]
    where a = render $ channelId channel
          b = render $ isLogging channel
          c = render $ isListening channel
 
instance QueryResults DBChannel where
  convertResults [fa, fb, fc] [va, vb, vc] = DBChannel a b c
    where a = convert fa va
          b = convert fb vb
          c = convert fc vc
  convertResults fs vs = convertError fs vs 3


instance QueryParams DBMessage where
   renderParams message = [a, b, c, d, e]
    where a = render $ body message
          b = render $ sender message
          c = render $ recipient message
          d = render $ mChannelId message
          e = render $ date message

instance QueryResults DBMessage where
  convertResults [fa, fb, fc, fd, fe] [va, vb, vc, vd, ve] = DBMessage a b c d e
    where a = convert fa va
          b = convert fb vb
          c = convert fc vc
          d = convert fd vd
          e = convert fe ve
  convertResults fs vs = convertError fs vs 5


instance QueryParams DBMetadata where
  renderParams metadata = [a]
    where a = render $ mdCurrentDay metadata

instance QueryResults DBMetadata where
  convertResults [fa] [va] = DBMetadata a
    where a = convert fa va
  convertResults fs vs = convertError fs vs 1




------------------------ QUERY AND CONNECTION -----------------------

-- TODO: you know... 
dbConnectInfo :: ConnectInfo
dbConnectInfo = defaultConnectInfo { connectHost     = "localhost",
                                     connectUser     = "llamabot",
                                     connectPassword = "llama",
                                     connectDatabase = "llamabot" }

dbConnect :: IO Connection
dbConnect = connect dbConnectInfo

dbClose :: Connection -> IO ()
dbClose = close

initializeDB :: Connection -> IO (DBMetadata)
initializeDB conn = do
  createAllTables conn
  initializeMetadata conn

initializeMetadata :: Connection -> IO (DBMetadata)
initializeMetadata conn = do
  mMetadata <- getMetadata conn
  case mMetadata of 
    Just metadata -> return metadata
    Nothing -> do
      currentDay <- getCurrentTime >>= return . utctDay
      let newMetadata = DBMetadata currentDay
      insertMetadata conn newMetadata
      return newMetadata


getMetadata :: Connection -> IO (Maybe DBMetadata)
getMetadata conn = do
  metadata <- selectMetadata conn
  
  case metadata of 
    [] -> return Nothing
    [m] -> return $ Just m
    xs -> return $ Just $ Prelude.head xs

-- TODO: figure out the more monadic way to do this with do
createAllTables :: Connection -> IO ()
createAllTables conn = do
  createUsersTable conn
  createChannelsTable conn
  createMessagesTable conn
  createMetadataTable conn
  putStrLn $ "-- all tables created or exist already"
  return ()

createUsersTable :: Connection -> IO ()
createUsersTable conn = void $ execute_ conn createUsersTableQuery

createChannelsTable :: Connection -> IO ()
createChannelsTable conn = void $ execute_ conn createChannelsTableQuery

createMessagesTable :: Connection -> IO ()
createMessagesTable conn = void $ execute_ conn createMessagesTableQuery 

createMetadataTable :: Connection -> IO ()
createMetadataTable conn = void $ execute_ conn createMetadataTableQuery


-- TODO: make partial insert queries for updates to single fields?
insertUser :: Connection -> DBUser -> IO ()
insertUser conn user = void $ execute conn insertUserQuery user

insertChannel :: Connection -> DBChannel -> IO ()
insertChannel conn channel = void $ execute conn insertChannelQuery channel

insertMessage :: Connection -> DBMessage -> IO ()
insertMessage conn message = void $ execute conn insertMessageQuery message

insertMetadata :: Connection -> DBMetadata -> IO ()
insertMetadata conn metadata = void $ execute conn insertMetadataQuery metadata

selectUsers :: Connection -> IO ([DBUser])
selectUsers conn = query_ conn "select * from users" 

selectUserById :: Connection -> Text -> IO (Maybe DBUser)
selectUserById conn uId = do
  userList <- query conn "select * from users where userId=?" ([uId])

  case userList of 
    [] -> return Nothing
    [u] -> return $ Just u
    xs -> return $ Just (Prelude.head xs)
      -- TODO: throw error instead? userId is a primary key so this will never happen
      

-- TODO: this is a n^2 op
--        even though we only query 2 at a time, we should try to improve
selectUsersByIds :: Connection -> [Text] -> IO (M.Map Text DBUser)
selectUsersByIds conn userIds = do
  dbUserList <- query conn "select * from users where userId in ?" $ (Only (In userIds))
  let generateUserMapEntry :: Text -> Maybe (Text, DBUser)
      generateUserMapEntry uId = case (L.find (\u -> uId == (userId u)) dbUserList) of
                                  Nothing -> Nothing
                                  Just u -> Just (uId, u)
  return $ M.fromList $ catMaybes $ Prelude.map generateUserMapEntry userIds

selectChannels :: Connection -> IO ([DBChannel])
selectChannels conn = query_ conn "select * from channels" 

selectMessages :: Connection -> IO ([DBMessage])
selectMessages conn = query_ conn "select * from messages"

selectMetadata :: Connection -> IO ([DBMetadata])
selectMetadata conn = query_ conn "select * from metadata"

createUsersTableQuery :: Query
createUsersTableQuery = 
  "CREATE TABLE IF NOT EXISTS users (\
     \ userId VARCHAR(32),\
     \ llamasSentToday SMALLINT(16),\
     \ llamasReceivedThisWeek SMALLINT(32),\
     \ llamasSentThisWeek SMALLINT(32),\
     \ llamasReceived SMALLINT(32),\
     \ llamasSent SMALLINT(32),\
     \ PRIMARY KEY (userId)\
     \);"

createChannelsTableQuery :: Query
createChannelsTableQuery = 
  "CREATE TABLE IF NOT EXISTS channels (\
     \ channelId VARCHAR(16),\
     \ isLogging BOOL,\
     \ isListening BOOL,\
     \ PRIMARY KEY (channelId)\
     \);"

createMessagesTableQuery :: Query
createMessagesTableQuery = 
  "CREATE TABLE IF NOT EXISTS messages (\
     \ body TEXT,\
     \ sender VARCHAR(32),\
     \ recipient VARCHAR(32),\
     \ channelId VARCHAR(16),\
     \ date DATE,\
     \ FOREIGN KEY (sender) REFERENCES users(userId),\
     \ FOREIGN KEY (recipient) REFERENCES users(userId),\
     \ FOREIGN KEY (channelId) REFERENCES channels(channelId)\
     \);"

createMetadataTableQuery :: Query
createMetadataTableQuery = 
  "CREATE TABLE IF NOT EXISTS metadata (\
    \ currentDay DATE);"


insertUserQuery :: Query
insertUserQuery = 
  "INSERT INTO users (userId, llamasSentToday, llamasReceivedThisWeek, llamasSentThisWeek, llamasReceived, llamasSent) VALUES (?, ?, ?, ?, ?, ?)"

insertChannelQuery :: Query
insertChannelQuery =
  "INSERT INTO channels (channelId, isLogging, isListening) VALUES (?, ?, ?)"

insertMessageQuery :: Query
insertMessageQuery = 
  "INSERT INTO messages (\
    \ body,\
    \ sender,\
    \ recipient,\
    \ channelId,\
    \ date)\
    \ VALUES (?, ?, ?, ?, ?);"

insertMetadataQuery :: Query
insertMetadataQuery =
  "INSERT INTO metadata (\
    \ currentDay)\
    \ VALUES (?);"
