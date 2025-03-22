{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}



import           Control.Concurrent
import           Control.Concurrent.STM

-- import           Control.Exception            hiding (Handler)
-- import           Control.Monad.IO.Class
import           Control.Monad
import           Data.Aeson
-- import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as C
import qualified Data.ByteString.Lazy         as BL
-- import           Data.Coerce
-- import           Data.Either
import           Data.Text                    as T
import           Data.Time
import           Data.Maybe

import           HFlags

import           Network.HTTP.Client          as NC
import qualified Network.HTTP.Client.TLS      as TLS
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.Wai                  as W
import           Network.Wai.Handler.Warp
import           Web.FormUrlEncoded


import           Llamabot
import           Slack



defineFlag "token" ("" :: String) "Token for sending requests to slack"
$(return [])


data IRForm = IRForm String deriving (Eq, Show)

instance FromForm IRForm where
  fromForm f = IRForm
    <$> parseUnique "payload" f


------------------------------- APP LOGIC ---------------------------------

decodeInteractionResponse :: BL.ByteString -> Either String InteractionResponse
--decodeInteractionResponse = eitherDecode =<< (BL.fromStrict . C.pack . coerce) =<< urlDecodeAsForm
decodeInteractionResponse b = 
  case (urlDecodeAsForm b) of
    Left str -> Left $ T.unpack str
    Right (IRForm st) -> 
        let mIr' = eitherDecode (BL.fromStrict $ C.pack st)
        in case mIr' of
            (Left err) -> Left err
            Right ir -> Right ir


app :: LlamaContextT -> W.Request -> (W.Response -> IO ResponseReceived) -> IO ResponseReceived
app contextT req responder = do 
  body <- strictRequestBody req
  let msg' = eitherDecode body
  case msg' of
    Left _ -> do
      let mIr = decodeInteractionResponse body
      case mIr of
        Left str -> error $ show str
        Right (InteractionResponse _ u _ _ as) -> do
         putStrLn $ " got an interaction response!!!! looks like " ++ (show $ uUsername u) ++ " clicked button val " ++ (show $ aValue $ Prelude.head as)
         putStrLn $ " not gonna do anything with it, because we don't care about these yet"
         responder $ responseLBS status200 [] BL.empty
    Right msg -> do
      case msg of
        Handshake _ ch _ -> do 
          putStrLn $ "got a handshake, responding with the challenge: " ++ show ch
          responder $ responseLBS status200 [(hContentType, "text/plain")] (BL.fromStrict $ C.pack $ show ch)
        (EventDetails _ _ _ ev _ _ _ _ _ _ _) -> do 
          
          checkAndUpdateDays contextT
          when (hasLlamasAndTag ev) $ do
            currentDay <- getCurrentTime >>= return . utctDay
            
            let (llamaTotal, recipient, body') = parseLlamaPost $ text ev
                channel' = fromMaybe defaultChannel (channel ev)
                llamaMessage = LlamaMessage { lmChannelId = channel'
                                           , lmSender = user ev
                                           , lmRecipient = recipient
                                           , lmTotal = llamaTotal
                                           , lmMsgBody = body'
                                           , lmTs = ts ev
                                           , lmDate = currentDay
                                           }
            _ <- forkIO $ handleLlamaResponse contextT llamaMessage 
            putStrLn $ "[INFO] Llama Message Received. Handler Thread Forked, Returning 200 OK"
          
          -- the 200 response
          responder $ responseLBS status200 [] BL.empty

 


defaultChannel :: Text
defaultChannel = "C08GV70G1GU" -- this is the #llamalog channel in LaunchLiveNow


-- TODO: should these be in the slack-api package? probably
ephemeralMessageSlackURL :: String
ephemeralMessageSlackURL = "https://slack.com/api/chat.postEphemeral"

messageSlackURL :: String
messageSlackURL = "https://slack.com/api/chat.postMessage"

  -- let msgBlocks = 
  --      [ BlockText (TextBlock "plain_text" False "Hello!")
  --      , BlockElements
  --          [ ButtonBody (TextBlock "plain_text" False "CLICK ME!") "primary" "left button"
  --          , ButtonBody (TextBlock "plain_text" False "No, CLICK ME!!!") "danger" "right button"
  --          ]
  --      ]



handleLlamaResponse :: LlamaContextT -> LlamaMessage -> IO ()
handleLlamaResponse contextT llamaMessage = do

  processResponse <- processLlamaMessage llamaMessage

  manager <- TLS.newTlsManager

  case processResponse of
    Left (MessageError errType errMsg) -> do
      llamaContext <- atomically $ readTVar contextT
      
      request <- generateMessageRequest ephemeralMessageSlackURL (lcToken llamaContext) errMsg llamaMessage
      reaction <- generateReactionRequest (lcToken llamaContext) "thinking_face" llamaMessage 
      _ <- httpLbs request manager
      _ <- httpLbs reaction manager
      putStrLn $ "[USER ERROR] Invalid Llama Message: " ++ (show errType)
    Right (MessageReply public hidden) -> do
      llamaContext <- atomically $ readTVar contextT
      
      publicRequest <- generateMessageRequest messageSlackURL (lcToken llamaContext) public llamaMessage
      hiddenRequest <- generateMessageRequest ephemeralMessageSlackURL (lcToken llamaContext) hidden llamaMessage

      reaction <- generateReactionRequest (lcToken llamaContext) "white_check_mark" llamaMessage 
      _ <- httpLbs publicRequest manager
      _ <- httpLbs hiddenRequest manager
      _ <- httpLbs reaction manager
      putStrLn "[INFO] Llama Message Processed"
     


-- they make us use a MonadThrow context for just generating the basic request
-- because it attaches it to IO actions or something, thus this IO dependency
-- but I'd love for it to be pure
generateMessageRequest :: String -> Text -> Text -> LlamaMessage -> IO (NC.Request)
generateMessageRequest url token reply llamaMessage = do
  initRequest <- parseRequest url
    
  let msg = PostMessage
            (lmChannelId llamaMessage)
            (Just reply)
            (Just $ lmSender llamaMessage)
            Nothing -- (Just thread)
            Nothing -- (Just msgBlocks)

  return $ initRequest 
             { NC.method = "POST"
             , NC.requestBody = RequestBodyLBS $ encode msg
             , NC.requestHeaders = 
               [ (hAuthorization, (C.pack $ "Bearer " ++ (T.unpack token)))
               , (hContentType, (C.pack "application/json"))
               ]
             }

generateReactionRequest :: Text -> Text -> LlamaMessage -> IO (NC.Request)
generateReactionRequest token emoji llamaMessage = do
  initRequest <- parseRequest "https://slack.com/api/reactions.add"
 
  let reaction = PostReaction (lmChannelId llamaMessage) emoji (lmTs llamaMessage)
  
  return $ initRequest 
             { NC.method = "POST"
             , NC.requestBody = RequestBodyLBS $ encode reaction
             , NC.requestHeaders = 
               [ (hAuthorization, (C.pack $ "Bearer " ++ (T.unpack token)))
               , (hContentType, (C.pack "application/json"))
               ]
             }
 

main :: IO ()
main = do
  _ <- $initHFlags "Lllamabot server v0.2"
 
  putStrLn "\n"
  putStrLn $ "Welcome to Llamabot v0.3"
  putStrLn "\n"
  putStrLn $ "OAUTH FLAG: " ++ (show $ flags_token)

  putStrLn $ "-- checking MySQL database connection (credentials hardcoded for now)"
  
  conn <- dbConnect

  putStrLn "-- database connection established, initializing tables if not yet existing"
  metadata <- initializeDB conn

  dbClose conn
  
  putStrLn "-- initializing program state"
  llamaT <- atomically $ initializeLlamaTVar (T.pack flags_token) metadata

  putStrLn "\n"
  putStrLn "starting server"
  putStrLn "listening on port 8081\n"


  run 8081 (app llamaT)



