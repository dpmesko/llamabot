{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}

module Slack.Message(
    PostMessage(..),
    PostReaction(..),
    Block(..),
    BlockType(..),
    BlockBody(..),
    TextBlock(..),
    Element(..),
    ElementType(..),
    ElementBody(..),
    Field(..)
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




-----------------------------------------------------------
------------------------ TYPES ---------------------------
data Field = Field
  { fType :: Text
  , fText :: Text
  } deriving (Eq, Show)


instance ToJSON Field where
  toJSON (Field ty te) = object
    [ "type" .= ty
    , "text" .= te
    ]


--------------------- ELEMENTS ----------------------------
data ElementType = Button | DatePicker deriving (Eq, Show)

data Element = Element
  { elementType :: ElementType
  , elementBody :: ElementBody
  } deriving (Eq, Show)

data ElementBody = 
    ButtonBody
      { bbText :: TextBlock
      , bbStyle :: Text
      , bbValue :: Text
      }
  | DatePickerBody
      { dpbActionId :: Text
      , dpbInitialDate :: Text
      , dpbPlaceholder :: Placeholder
      } deriving (Eq, Show)

data Placeholder = Placeholder
  { pType :: Text
  , pText :: Text
  } deriving (Eq, Show)




instance ToJSON Placeholder where
  toJSON (Placeholder ty te) = object
    [ "type" .= ty
    , "text" .= te
    ]

instance ToJSON Element where
  toJSON (Element Button (ButtonBody t s v)) = object
    [ "type" .= String "button"
    , "text" .= t
    , "style" .= s
    , "value" .= v
    ]
  toJSON (Element DatePicker (DatePickerBody a i p)) = object
    [ "type" .= String "datepicker"
    , "action_id" .= a
    , "initial_date" .= i
    , "placeholder" .= p
    ]
  toJSON x = error $ "you tried to JSON encode a bad pair of element type and element body: " ++ (show x)


------------------ BLOCKS ---------------------------
data TextBlock = TextBlock
  { tbType :: Text
  , tbEmoji :: Bool
  , tbText :: Text
  } deriving (Eq, Show)


data BlockType = Header | Section | Actions deriving (Eq, Show)
data BlockBody = 
    BlockText TextBlock
  | BlockFields [Field]
  | BlockElements [Element] deriving (Eq, Show)


data Block = Block
  { blockType :: BlockType
  , blockBody :: BlockBody
  } deriving (Eq, Show)




instance ToJSON TextBlock where
  toJSON (TextBlock ty em te) = object
    [ "type" .= ty
    , "emoji" .= em
    , "text" .= te
    ]

instance ToJSON Block where
  toJSON (Block (Header) (BlockText tb)) = object
    [ "type" .= String "header"
    , "text" .= tb
    ]
  toJSON (Block (Section) (BlockFields fd)) = object
    [ "type" .= String "section"
    , "fields" .= fd
    ]
  toJSON (Block (Actions) (BlockElements be)) = object
    [ "type" .= String "actions"
    , "elements" .= be
    ]
  toJSON x = error $ "invalid block construction to encode as JSON: " ++ (show x)



------------------- the WHOLE message -------------
data PostMessage = PostMessage
  { pmChannel :: Text
  , pmText :: Maybe Text
  , pmThreadTs :: Maybe Text
  , blocks :: Maybe [Block]
  } deriving (Eq, Show)


instance ToJSON PostMessage where
  toJSON (PostMessage c t ts bs) = object
    [ "channel" .= c
    , "text" .= t
    , "thread_ts" .= ts
    , "blocks" .= bs
    ]


data PostReaction = PostReaction
  { prChannel :: Text
  , prName :: Text
  , prTs :: Text
  } deriving (Eq, Show)

instance ToJSON PostReaction where
  toJSON (PostReaction c n t) = object
    [ "channel" .= c
    , "name" .= n
    , "timestamp" .= t
    ]
