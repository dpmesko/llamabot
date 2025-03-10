{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}


module Llamabot.Context (
  LlamaContextT,
  LlamaContext(..),
  initializeLlamaTVar
  ) where


import           Data.Text
import           Control.Concurrent.STM

import           Llamabot.Database


type LlamaContextT = TVar LlamaContext


---------------------------------------------------

data LlamaContext = LlamaContext
  { lcToken         :: Text
  , lcMetadata      :: DBMetadata
  } deriving (Eq, Show)


initializeLlamaTVar :: Text -> DBMetadata -> STM LlamaContextT
initializeLlamaTVar token metadata = newTVar $ initializeLlamaContext token metadata

initializeLlamaContext :: Text -> DBMetadata -> LlamaContext
initializeLlamaContext token metadata = LlamaContext
  { lcToken = token
  , lcMetadata = metadata
  }



