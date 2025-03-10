{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}


module Slack.Events(
    CallbackType(..),
    Authorization(..),
    Event(..),
    Message(..)
  ) where

  


-- import           Control.Exception            hiding (Handler)
-- import           Control.Monad.IO.Class
import           Control.Monad
import           Data.Aeson
-- import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as C
import qualified Data.ByteString.Lazy         as BL
-- import           Data.Maybe
import           Data.Text                    as T
-- import           Servant







--------------------------------- EVENTS API --------------------------------
data CallbackType = URLVerification | EventCallback deriving (Show, Eq)

data Authorization = Authorization
  { aEnterpriseId :: Maybe Text
  , aTeamId :: Text
  , aUserId :: Text
  , aIsBot :: Bool
  } deriving (Show, Eq)


data Event = Event
  { typ :: Text
  , channel :: Maybe Text
  , text :: Maybe Text
  , eventTimestamp :: Text
  , user :: Text
  , ts :: Text
  , item :: Maybe Text
  } deriving (Show, Eq)


data Message =
    Handshake 
      { token :: Text
      , challenge :: Text
      , htyp :: CallbackType
      }
  | EventDetails
      { token :: Text
      , teamId :: Text
      , apiAppId :: Text
      , event :: Event
      , etyp :: CallbackType
      , authedUsers :: Maybe Text
      , authedTeams :: Maybe Text
      , authorizations :: [Authorization]
      , eventContext :: Text
      , eventId :: Text
      , eventTime :: Integer
      } deriving (Show, Eq)


instance FromJSON CallbackType where
  parseJSON (String str) = case str of
      "url_verification" -> return URLVerification
      "event_callback" -> return EventCallback
      x -> error $ "unknown callback type: " ++ show x
  parseJSON x = error $ "unknown callback type: " ++ show x


instance FromJSON Authorization where
  parseJSON (Object o) = do
    ei <- o .:? "enterprise_id"
    ti <- o .: "team_id"
    ui <- o .: "user_id"
    ib <- o .: "is_bot"
    return $ Authorization ei ti ui ib
  parseJSON x = error $ "unknown authorizations type: " ++ show x
     
instance FromJSON Event where
  parseJSON (Object o) = do
    tp <- o .: "type"
    ch <- o .:? "channel"
    tx <- o .:? "text"
    es <- o .: "event_ts"
    us <- o .: "user"
    ts <- o .: "ts"
    it <- o .:? "item"
    return $ Event tp ch tx es us ts it
  parseJSON x = error $ "unknown event type: " ++ show x

instance FromJSON Message where
  parseJSON (Object o) = do
    tp <- o .: "type"
    case tp of
      URLVerification -> do
        to <- o .: "token"
        ch <- o .: "challenge"
        return $ Handshake to ch tp
      EventCallback -> do
        to <- o .: "token"
        ti <- o .: "team_id"
        ai <- o .: "api_app_id"
        ev <- o .: "event"
        au <- o .:? "authed_users"
        at <- o .:? "authed_teams"
        as <- o .: "authorizations"
        ec <- o .: "event_context"
        ei <- o .: "event_id"
        et <- o .: "event_time"
        return $ EventDetails to ti ai ev tp au at as ec ei et
  parseJSON x = error $ "unknown message type " ++ show x
