{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE DeriveGeneric    #-}

module Slack.PostMessage(
    PostMessage(..),
    PostReaction(..),
    Block(..),
    Blocks(..),
    BlockType(..),
    TextBlock(..),
    Element(..),
    ElementType(..),
    RichTextElement(..),
    RichTextListStyle(..),
    Field(..)
  ) where


import           Data.Aeson
import           Data.Text                    as T

import           GHC.Generics


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

data RichTextElement =
    RichTextSection { rteElements :: [RichTextElement] }
  | RichTextText { rttText :: Text }
  | RichTextUser { rtuUserId :: Text } 
  | RichTextList
      { rtlStyle    :: RichTextListStyle
      , rtlElements :: [RichTextElement]
      } deriving (Eq, Show)

data RichTextListStyle = Bullet | Ordered deriving (Eq, Show)

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

instance ToJSON RichTextElement where
  toJSON (RichTextSection es) = object
    [ "type" .= String "rich_text_section"
    , "elements" .= es
    ]
  toJSON (RichTextText t) = object
    [ "type" .= String "text"
    , "text" .= t
    ]
  toJSON (RichTextUser uid) = object
    [ "type" .= String "user"
    , "user_id" .= uid
    ]
  toJSON (RichTextList s es) = object
    [ "type" .= String "rich_text_list"
    , "style" .= s
    , "elements" .= es
    ]

instance ToJSON RichTextListStyle where
  toJSON Bullet = String "bullet"
  toJSON Ordered = String "ordered"



------------------ BLOCKS ---------------------------
data TextBlock = TextBlock
  { tbType :: Text
  , tbText :: Text
  } deriving (Eq, Show, Generic)


data BlockType = Header | Section | Actions | RichTextBlock deriving (Eq, Show)
data Block = 
    BlockText TextBlock
  | BlockFields [Field]
  | BlockElements [Element]
  | BlockRichText [RichTextElement]
  deriving (Eq, Show)

data Blocks = Blocks Text [Block] deriving (Eq, Show)

instance ToJSON TextBlock where
  toJSON (TextBlock ty te) = object
    [ "type" .= ty
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
  toJSON (BlockRichText brt) = object
    [ "type" .= String "rich_text"
    , "elements" .= brt
    ]
--  toJSON x = error $ "invalid block construction to encode as JSON: " ++ (show x)

instance ToJSON Blocks where
  toJSON (Blocks txt blocks) = object
    [ "text" .= txt
    , "blocks" .= blocks 
    ]

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
