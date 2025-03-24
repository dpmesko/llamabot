{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}



import           Control.Concurrent
import           Control.Concurrent.STM

-- import           Control.Exception            hiding (Handler)
import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.Aeson
import qualified Data.ByteString.Char8        as C
import qualified Data.ByteString.Lazy         as BL
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
defineFlag "port" (8081 :: Int) "Port number on which the app server will listen"
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
  
  runLoggingT $ do

    let msg' = eitherDecode body
    case msg' of
      Left _ -> do
        let mIr = decodeInteractionResponse body
        case mIr of
          Left str -> error $ show str
          Right (InteractionResponse _ u _ _ as) -> do
            $logInfoS "Received Interaction Response" $ T.pack $ (show $ uUsername u) ++ 
              " clicked button val " ++ (show $ aValue $ Prelude.head as)
            $logInfoS "Received InteractionResponse" "Nothing to do with this" 
            lift $ responder $ responseLBS status200 [] BL.empty
      Right msg -> do
        case msg of
          Handshake _ ch _ -> do 
            $logInfoS "app" "Responding to handshake with challenge"
            lift $ responder $ responseLBS status200 [(hContentType, "text/plain")] (BL.fromStrict $ C.pack $ show ch)
          (EventDetails _ _ _ ev _ _ _ _ _ _ _) -> do 
          
            checkAndUpdateDays contextT
            when (hasLlamasAndTag ev) $ do
              currentDay <- lift $ getCurrentTime >>= return . utctDay
            
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
              void $ lift $ forkIO $ runLoggingT $ handleLlamaResponse contextT llamaMessage 
              $logInfoS "app" "Llama Message Received - Handler Thread Forked, Returning 200 OK"
            -- the 200 response
            lift $ responder $ responseLBS status200 [] BL.empty

 


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



handleLlamaResponse :: LlamaContextT -> LlamaMessage -> LoggingT IO ()
handleLlamaResponse contextT llamaMessage = do

  processResponse <- lift $ processLlamaMessage llamaMessage

  manager <- lift $ TLS.newTlsManager

  case processResponse of
    Left (MessageError errType errMsg) -> do
      lift $ do
        llamaContext <- atomically $ readTVar contextT
        request <- generateMessageRequest ephemeralMessageSlackURL (lcToken llamaContext) errMsg llamaMessage
        reaction <- generateReactionRequest (lcToken llamaContext) "thinking_face" llamaMessage 
        void $ httpLbs request manager
        void $ httpLbs reaction manager
      $logErrorS "handleLlamaResponse" $ T.pack $ "Invalid Llama Message: " ++ (show errType)

    Right (MessageReply public hidden) -> do
      lift $ do
        llamaContext <- atomically $ readTVar contextT
      
        publicRequest <- generateMessageRequest messageSlackURL (lcToken llamaContext) public llamaMessage
        hiddenRequest <- generateMessageRequest ephemeralMessageSlackURL (lcToken llamaContext) hidden llamaMessage

        reaction <- generateReactionRequest (lcToken llamaContext) "white_check_mark" llamaMessage 
        void $ httpLbs publicRequest manager
        void $ httpLbs hiddenRequest manager
        void $ httpLbs reaction manager
      $logInfoS "handleLlamaResponse" "Llama Message Processed"



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
  putStrLn $ "OAUTH TOKEN: " ++ (show $ flags_token)
  putStrLn $ "PORT: " ++ (show $ flags_port)

  putStrLn $ "-- checking MySQL database connection (credentials hardcoded for now)"
  
  conn <- dbConnect

  putStrLn "-- database connection established, initializing tables if not yet existing"
  metadata <- initializeDB conn

  dbClose conn
  
  putStrLn "-- initializing program state"
  llamaT <- atomically $ initializeLlamaTVar (T.pack flags_token) metadata

  putStrLn "\n"
  putStrLn "starting server"
  putStrLn $ "listening on port " ++ (show flags_port) ++ "\n"

  run flags_port (app llamaT)



