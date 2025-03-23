{-# LANGUAGE OverloadedStrings  #-}


module Slack.SlashCommands(
  SlashCommand(..),
  decodeSlashCommand
  ) where


import qualified Data.ByteString.Lazy   as BL
import           Data.Text
import           Web.FormUrlEncoded



data SlashCommand = SlashCommand
  { scChannelId   :: Text
  , scUserId      :: Text
  , scCommand     :: Text
  , scText        :: Text
  , scResponseUrl :: Text
  } deriving (Eq, Show)

instance FromForm SlashCommand where
  fromForm f = SlashCommand
    <$> parseUnique "channel_id" f
    <*> parseUnique "user_id" f
    <*> parseUnique "command" f
    <*> parseUnique "text" f
    <*> parseUnique "response_url" f

decodeSlashCommand :: BL.ByteString -> Either Text SlashCommand
decodeSlashCommand b = urlDecodeAsForm b
