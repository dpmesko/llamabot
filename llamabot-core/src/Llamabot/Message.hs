{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}


module Llamabot.Message (
  LlamaMessage(..),
  checkAndUpdateDays,
  defaultDailyAllotment,
  parseLlamaPost,
  hasLlamasAndTag,
  processLlamaMessage
  ) where


import           Control.Concurrent.STM
import           Control.Monad.Trans.Class

import qualified Data.Map                     as M
import           Data.Time
import           Data.Time.Calendar.WeekDate
import           Data.Text                    as T

import           Llamabot.Context
import           Llamabot.Database
import           Llamabot.Logger
import           Llamabot.Response

import           Slack


-- TODO: configurable in env
defaultDailyAllotment :: Integer
defaultDailyAllotment = 5




checkAndUpdateDays :: LlamaContextT -> LoggingT IO ()
checkAndUpdateDays contextT = do
  llamaContext <- lift $ atomically $ readTVar contextT

  currentDay <- lift $ getCurrentTime >>= return . utctDay
  let contextDay = mdCurrentDay $ lcMetadata llamaContext
  
  if currentDay > contextDay 
    then do
      $logInfoS "checkAndUpdateDays" $ T.pack $ "First Message Today: " ++ (show currentDay) ++ 
        ", Last Message Sent " ++ (show contextDay)
      $logInfoS "checkAndUpdateDays" "Updating currentDay Metadata and Resetting Daily Allotments"

      lift $ do
        dbConn <- dbConnect
        resetDailyAllotments dbConn defaultDailyAllotment
        updateMetadata dbConn $ DBMetadata currentDay
        dbClose dbConn 
      checkAndUpdateNewWeek currentDay contextDay

      let newContext = llamaContext { lcMetadata = DBMetadata currentDay }
      lift $ atomically $ writeTVar contextT newContext
  else return ()

checkAndUpdateNewWeek :: Day -> Day -> LoggingT IO ()
checkAndUpdateNewWeek currentDay previousDay = do
  let (_, currentDayWeek, _) = toWeekDate currentDay
      (_, previousDayWeek, _) = toWeekDate previousDay
  if currentDayWeek > previousDayWeek
    then do
      $logInfoS "checkAndUpdateNewWeek" "New Week Beginning - Updating Weekly Totals"

      lift $ do
        dbConn <- dbConnect
        resetWeeklyTotals dbConn
        dbClose dbConn
  else return ()



-- TODO: kind of ugly string nonsense, see if we can clean
parseLlamaPost :: Maybe Text -> (Integer, Text, Text)
parseLlamaPost Nothing = (0, "", "")
parseLlamaPost (Just t) = 
  let llamaTotal = toInteger $ Prelude.length $ T.breakOnAll ":llama:" t
      recipient =
        let prstr = snd $ Prelude.head $ T.breakOnAll "<@" t
        in T.drop 2 $ fst $ Prelude.head $ T.breakOnAll ">" prstr
  in (llamaTotal, recipient, t)

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
        | s == r = Left $ generateError 0 SelfSend -- Integer ignored for SelfSend
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
                                     , llamaDailyAllotment    = defaultDailyAllotment
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
                                        , llamaDailyAllotment    = defaultDailyAllotment
                                        , llamasReceivedThisWeek = 0
                                        , llamasSentThisWeek     = 0
                                        , llamasReceived         = 0
                                        , llamasSent             = 0
                                        }
                   insertUser dbConn newUser
                   return newUser

  dbClose dbConn
  return (sender, recipient)


senderHasAllotment :: DBUser -> Integer -> Bool
senderHasAllotment sender amount = (llamaDailyAllotment sender) - amount >= 0

updateSenderTotals :: DBUser -> Integer -> IO (DBUser)
updateSenderTotals sender@DBUser{..} amount = do
  let newAllotment = llamaDailyAllotment - amount
      newWeekTotal = llamasSentThisWeek + amount
      newTotal = llamasSent + amount
      sender' = sender { llamaDailyAllotment = newAllotment
                       , llamasSentThisWeek = newWeekTotal
                       , llamasSent = newTotal
                       }
      senderUpdate = SenderUpdate { allotment = newAllotment
                                  , sentThisWeek = newWeekTotal
                                  , sent = newTotal
                                  , senderId = userId
                                  }
  dbConn <- dbConnect
  (updateSender dbConn senderUpdate) >> (dbClose dbConn) >> return sender'


updateRecipientTotals :: DBUser -> Integer -> IO ()
updateRecipientTotals DBUser{..} amount = do
  let newWeekTotal = llamasReceivedThisWeek + amount
      newTotal = llamasReceived + amount
      recipientUpdate = RecipientUpdate { receivedThisWeek = newWeekTotal
                                        , received = newTotal
                                        , recipientId = userId
                                        }
  dbConn <- dbConnect
  (updateRecipient dbConn recipientUpdate) >> dbClose dbConn




addMessage :: LlamaMessage -> IO ()
addMessage message = do
  let dbMessage = messageToDBMessage message
  dbConn <- dbConnect
  insertMessage dbConn dbMessage
  dbClose dbConn

messageToDBMessage :: LlamaMessage -> DBMessage
messageToDBMessage LlamaMessage{..} = DBMessage { body = lmMsgBody
                                                , sender = lmSender
                                                , recipient = lmRecipient
                                                , msgChannelId = lmChannelId
                                                , date = lmDate
                                                }


processLlamaMessage :: LlamaMessage -> IO (ParserResult)
processLlamaMessage message = do
  -- TODO: update DB for new day/week logic
  -- and maybe, that shouldn't happen for all users at once, but per user? 

  -- llamaContext <- atomically $ readTVar contextT
  -- currentDay <- getCurrentTime >>= return . utctDay
  
  -- add channel if needed, also maybe these things should happen on any slack event

  let validity = checkBasicValidity message
  
  case validity of
    Left err -> return $ Left err
    Right _ -> do
      (sender, recipient) <- fetchOrCreateUsers (lmSender message) (lmRecipient message)
      if not $ senderHasAllotment sender (lmTotal message) 
        then return $ Left $ generateError (llamaDailyAllotment sender) ExceedsAllotment
      else do
        sender' <- updateSenderTotals sender (lmTotal message)
        updateRecipientTotals recipient (lmTotal message)
        addMessage message
        return $ Right $ generateReply message sender' 

