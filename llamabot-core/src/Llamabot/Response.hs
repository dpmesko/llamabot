{-# LANGUAGE OverloadedStrings  #-}


module Llamabot.Response (
  LlamaMessage(..),
  ParserResult,
  MessageReply(..),
  MessageError(..),
  MessageErrorType(..),
  generateError,
  generateReply
  ) where


import           Data.Text           as T
import           Data.Time

import           Llamabot.Database   



-- This type is more about the message than the response
-- but we couldn't put it in Message.hs cus we need it here
-- and Message.hs imports this module.
--
-- Maybe it can be moved to a new Types.hs module eventually?

data LlamaMessage = LlamaMessage
  { lmChannelId :: Text
  , lmSender    :: Text
  , lmRecipient :: Text
  , lmTotal     :: Integer
  , lmMsgBody   :: Text
  , lmTs        :: Text
  , lmDate      :: Day
  } deriving (Eq, Show)




type ParserResult = Either MessageError MessageReply

data MessageReply = MessageReply 
  { mrPublicReply :: Text
  , mrHiddenReply :: Text
  } deriving (Eq, Show)

data MessageError = MessageError MessageErrorType Text deriving (Eq, Show)

data MessageErrorType = ExceedsAllotment
                      | SelfSend
                      deriving (Eq, Show)



-- TODO: add some random selection of reply template
--        will require randomness, IO dependency somewhere
--

llamaPlural :: String
llamaPlural = " llamas"

llamaSingular :: String
llamaSingular = " llama"

generateReply :: LlamaMessage -> DBUser -> MessageReply
generateReply message sendingUser = 
  let public = 
        if (lmTotal message) == 1
          then T.pack $ "<@" ++ (T.unpack $ lmSender message) ++ "> sent <@" ++
               (T.unpack $ lmRecipient message) ++ "> " ++ (show $ lmTotal message) ++
               llamaSingular ++ " :muscle: :star-struck:"
        else T.pack $ "<@" ++ (T.unpack $ lmSender message) ++ "> sent <@" ++
               (T.unpack $ lmRecipient message) ++ "> " ++ (show $ lmTotal message) ++
               llamaPlural ++ " :muscle: :star-struck:"
      
      hidden =
        if (llamaDailyAllotment sendingUser) == 1
          then T.pack $ "You have " ++ (show $ llamaDailyAllotment sendingUser) ++ 
               llamaSingular ++ " remaining today."
        else T.pack $ "You have " ++ (show $ llamaDailyAllotment sendingUser) ++ 
               llamaPlural ++ " remaining today."
  
  in MessageReply public hidden

-- TODO: more generic/algebraic way? (Integer only used for exceedsallotment)
generateError :: Integer -> MessageErrorType -> MessageError
generateError currentAllotment ExceedsAllotment = MessageError ExceedsAllotment $ T.pack $
  "*OOPS* - Too many llamas! You only have " ++ (show currentAllotment) ++ " left in your daily allotment."
generateError _ SelfSend = MessageError SelfSend $ 
  "*OOPS* - You cannot send llamas to yourself! They work better when you give them away :grin:"


