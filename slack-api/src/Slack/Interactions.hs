{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}



module Slack.Interactions(
    InteractionResponse(..)
  , User(..)
  , Channel(..)
  , Action(..)
  ) where



import           Data.Aeson
import           Data.Text                    as T


-- NOTE: there are tons of fields missing from the actual return types, because I don't need them for llamabot
data InteractionResponse = InteractionResponse
  { irType        :: Text
  , irUser        :: User
  , irChannel     :: Channel 
  , irResponseURL :: Text
  , irActions     :: [Action]
  } deriving (Eq, Show)


data User = User
  { uId       :: Text
  , uUsername :: Text
  , uName     :: Text
  , uTeamId   :: Text
  } deriving (Eq, Show)

data Channel = Channel
  { cId       :: Text
  , cName     :: Text
  } deriving (Eq, Show)


data Action = Action
  { aActionId :: Text
  , aBlockId  :: Text
  , aValue    :: Text
  , aActionTs :: Text
  } deriving (Eq, Show)



instance FromJSON InteractionResponse where
  parseJSON (Object o) = do
    t <- o .: "type"
    u <- o .: "user"
    c <- o .: "channel"
    r <- o .: "response_url"
    a <- o .: "actions"
    return $ InteractionResponse t u c r a
  parseJSON x = error $ "unknown interaction response type: " ++ show x
    

instance FromJSON User where
  parseJSON (Object o) = do
    i <- o .: "id"
    u <- o .: "username"
    n <- o .: "name"
    t <- o .: "team_id"
    return $ User i u n t
  parseJSON x = error $ "unknown user type: " ++ show x

instance FromJSON Channel where
  parseJSON (Object o) = do
    i <- o .: "id"
    n <- o .: "name"
    return $ Channel i n
  parseJSON x = error $ "unknown channel type: " ++ show x
 

instance FromJSON Action where
  parseJSON (Object o) = do
    a <- o .: "action_id"
    b <- o .: "block_id"
    v <- o .: "value"
    t <- o .: "action_ts"
    return $ Action a b v t
  parseJSON x = error $ "unknown action type: " ++ show x
