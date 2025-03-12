{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}


module Llamabot.Message (
  LlamaMessage(..),
  parseLlamaPost,
  hasLlamasAndTag,
  processLlamaMessage
  ) where



import qualified Data.Map           as M
import           Data.Text          as T

import           Llamabot.Database

import           Slack


data LlamaMessage = LlamaMessage
  { lmChannelId :: Text
  , lmSender    :: Text
  , lmRecipient :: Text
  , lmTs        :: Text
  , lmTotal     :: Integer
  } deriving (Eq, Show)


type ParserResult = Either MessageError MessageResult

data MessageResult = MessageResult Text deriving (Eq, Show)

data MessageError = MessageError Text deriving (Eq, Show)

data MessageErrorType = ExceedsAllotment
                      | SelfSend
                      deriving (Eq, Show)




-- TODO: kind of ugly string nonsense, see if we can clean
parseLlamaPost :: Maybe Text -> (Integer, Text)
parseLlamaPost Nothing = (0, "")
parseLlamaPost (Just t) = 
  let llamaTotal = toInteger $ Prelude.length $ T.breakOnAll ":llama:" t
      recipient =
        let prstr = snd $ Prelude.head $ T.breakOnAll "<@" t
        in T.drop 2 $ fst $ Prelude.head $ T.breakOnAll ">" prstr
  in (llamaTotal, recipient)

hasLlamasAndTag :: Event -> Bool
hasLlamasAndTag ev = (countLlamas ev > 0) && (countTags ev == 1)

countTags :: Event -> Integer
countTags Event{..} = 
  case text of
    Nothing -> 0
    Just t -> toInteger $ Prelude.length $ T.breakOnAll "<@" t


countLlamas :: Event -> Integer
countLlamas Event{..} =
  case text of
    Nothing -> 0
    Just t -> toInteger $ Prelude.length $ T.breakOnAll ":llama:" t


-- I want this function to shortcircuit when it encounters one error,
-- which is why I use Either as a monad, but I'm not interested in the Right
-- value ever, so I use a Bool always set to True
-- TODO: this is probably not paradigmatic, but I wanted to move on rather
--       than try to find the cleanest solution right now
checkBasicValidity :: LlamaMessage -> Either MessageError Bool
checkBasicValidity message = 

  -- using let-bound functions, assuming we will eventually have more
  --  ...like tooManyTags? we are too far down the parsing here for that though,
  --     will require a rethink
  
  let selfSend :: Text -> Text -> Either MessageError Bool
      selfSend s r
        | s == r = Left $ generateError message SelfSend
        | otherwise = Right True
  in do
    _ <- selfSend (lmSender message) (lmRecipient message)
    return True


fetchOrCreateUsers :: Text -> Text -> IO (DBUser, DBUser)
fetchOrCreateUsers senderId recipientId = do

  dbConn <- dbConnect
  usersMap <- selectUsersByIds dbConn [senderId, recipientId]
  sender <- case M.lookup senderId usersMap of
              Just user -> return user
              Nothing -> do
                let newUser = DBUser { userId                 = senderId
                                     , llamasSentToday        = 0 
                                     , llamasReceivedThisWeek = 0
                                     , llamasSentThisWeek     = 0 
                                     , llamasReceived         = 0
                                     , llamasSent             = 0
                                     }
                insertUser dbConn newUser
                return newUser
  
  recipient <- case M.lookup recipientId usersMap of
                 Just user -> return user
                 Nothing -> do
                   let newUser = DBUser { userId                 = recipientId
                                        , llamasSentToday        = 0
                                        , llamasReceivedThisWeek = 0
                                        , llamasSentThisWeek     = 0
                                        , llamasReceived         = 0
                                        , llamasSent             = 0
                                        }
                   insertUser dbConn newUser
                   return newUser
  
  return (sender, recipient)


-- TODO: make allotment configurable and stored in context?
senderHasAllotment :: DBUser -> Integer -> Bool
senderHasAllotment sender amount = (5 - (llamasSentToday sender)) >= amount

updateSenderTotals :: DBUser -> Integer -> IO ()
updateSenderTotals _ _ = return ()
--updateSenderTotals sender amount = return ()

updateRecipientTotals :: DBUser -> Integer -> IO ()
-- updateRecipientTotals recipient amount = return ()
updateRecipientTotals _ _ = return ()


generateReply :: LlamaMessage -> MessageResult
generateReply _ = MessageResult "bla bla for now"

generateError :: LlamaMessage -> MessageErrorType -> MessageError
generateError _ _ = MessageError "you stink for now"


processLlamaMessage :: LlamaMessage -> IO (ParserResult)
processLlamaMessage message = do
  -- TODO: update DB for new day/week logic
  -- llamaContext <- atomically $ readTVar contextT
  -- currentDay <- getCurrentTime >>= return . utctDay
  
  let validity = checkBasicValidity message
  
  putStrLn $ "validity - " ++ (show validity)
  
  case validity of
    Left err -> return $ Left err
    Right _ -> do
      (sender, recipient) <- fetchOrCreateUsers (lmSender message) (lmRecipient message)
      if not $ senderHasAllotment sender (lmTotal message) 
        then return $ Left $ generateError message ExceedsAllotment
      else do
        updateSenderTotals sender (lmTotal message)
        updateRecipientTotals recipient (lmTotal message)
        return $ Right $ generateReply message 

