{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}

module Slack.PostMessage(
    PostMessage(..),
    PostReaction(..),
    Block(..),
    BlockType(..),
    TextBlock(..),
    Element(..),
    ElementType(..),
    Field(..)
  ) where


import           Data.Aeson
import           Data.Text                    as T




------------------------ FIELDS ---------------------------
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
data Element = 
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
  toJSON (ButtonBody t s v) = object
    [ "type" .= String "button"
    , "text" .= t
    , "style" .= s
    , "value" .= v
    ]
  toJSON (DatePickerBody a i p) = object
    [ "type" .= String "datepicker"
    , "action_id" .= a
    , "initial_date" .= i
    , "placeholder" .= p
    ]
--  toJSON x = error $ "you tried to JSON encode a bad pair of element type and element body: " ++ (show x)


------------------ BLOCKS ---------------------------
data TextBlock = TextBlock
  { tbType :: Text
  , tbEmoji :: Bool
  , tbText :: Text
  } deriving (Eq, Show)


data BlockType = Header | Section | Actions deriving (Eq, Show)
data Block = 
    BlockText TextBlock
  | BlockFields [Field]
  | BlockElements [Element]
  deriving (Eq, Show)



instance ToJSON TextBlock where
  toJSON (TextBlock ty em te) = object
    [ "type" .= ty
    , "emoji" .= em
    , "text" .= te
    ]

instance ToJSON Block where
  toJSON (BlockText tb) = object
    [ "type" .= String "header"
    , "text" .= tb
    ]
  toJSON (BlockFields fd) = object
    [ "type" .= String "section"
    , "fields" .= fd
    ]
  toJSON (BlockElements be) = object
    [ "type" .= String "actions"
    , "elements" .= be
    ]
--  toJSON x = error $ "invalid block construction to encode as JSON: " ++ (show x)



------------------- the WHOLE Message -------------
data PostMessage = PostMessage
  { pmChannel :: Text
  , pmText :: Maybe Text
  , pmUser :: Maybe Text
  , pmThreadTs :: Maybe Text
  , blocks :: Maybe [Block]
  } deriving (Eq, Show)


instance ToJSON PostMessage where
  toJSON (PostMessage c t u ts bs) = object
    [ "channel" .= c
    , "text" .= t
    , "user" .= u
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
