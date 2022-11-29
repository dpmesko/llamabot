{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}


module Llamabot.Types (
  LlamaContextT,
  LlamaContext(..),
  ActiveChannel(..),
  ProcessedMessage(..),
  ActiveUser(..),
  initializeLlamaTVar,
  ) where

-- import           Control.Monad.IO.Class
-- import           Control.Monad
-- import           Control.Monad.State.Lazy
import           Data.Map.Strict          as M
import           Data.Text

import           Control.Concurrent.STM



type LlamaContextT = TVar LlamaContext

type UserID = Text

---------------------------------------------------

data LlamaContext = LlamaContext
  { lcToken    :: Text
  -- eventually, these three lists below become DB tables and we store a DBhandle
  , lcChannels :: [ActiveChannel]
  , lcMessages :: [ProcessedMessage]
  , lcUsers    :: Map UserID ActiveUser
  } deriving (Eq, Show)


data ActiveChannel = ActiveChannel
  { acId     :: Text
  , acLog    :: Bool
  , acListen :: Bool
  } deriving (Eq, Show)


data ProcessedMessage = ProcessedMessage 
  { pmMessage     :: Text
  , pmSenderID    :: UserID
  , pmRecipientID :: UserID
  , pmChannelID   :: Text
  , pmTotal       :: Integer
  } deriving (Eq, Show)

data ActiveUser = ActiveUser
  { auId                     :: UserID
  , auLlamasSentToday        :: Integer
  , auLlamasReceivedThisWeek :: Integer
  , auLlamasReceived         :: Integer
  } deriving (Eq, Show)


initializeLlamaTVar :: Text -> STM LlamaContextT
initializeLlamaTVar = newTVar . initializeLlamaContext

initializeLlamaContext :: Text -> LlamaContext
initializeLlamaContext token = LlamaContext
  { lcToken = token
  , lcChannels = []
  , lcMessages = []
  , lcUsers = M.empty
  }
