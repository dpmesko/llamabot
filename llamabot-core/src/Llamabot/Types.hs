{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}

import           Control.Monad.IO.Class
import           Control.Monad
import           Control.Monad.State.Lazy




type LlamaM = StateT LlamaEnv IO


---------------------------------------------------

data LlamaEnv = LlamaEnv
  { leToken    :: Text
  -- eventually, these three lists below become DB tables and we store a DBhandle
  , leChannels :: [StoredChannel]
  , leMessages :: [StoredMessage]
  , leUsers    :: [StoredUser]
  } deriving (Eq, Show)


data StoredChannel = StoredChannel
  { lcId     :: Text
  , lcLog    :: Bool
  , lcListen :: Bool
  } deriving (Eq, Show)


data StoredMessage = StoredMessage 
  { lmMessage :: Text
  , lmUserID  :: Text
  , lmTotal   :: Integer
  } deriving (Eq, Show)

data StoredUser = StoredUser
  { suId                     :: Text
  , suLlamasSentToday        :: Integer
  , suLlamasReceivedThisWeek :: Integer
  , suLlamasReceived         :: Integer
  } deriving (Eq, Show)


initializeLlamaEnv :: Text -> LlamaEnv
initializeLlamaEnv token = LlamaEnv
  { leToken = token
  , leChannels = []
  , leMessages = []
  , leUsers = []
  }
