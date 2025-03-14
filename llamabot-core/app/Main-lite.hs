{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}

-- import           Control.Exception            hiding (Handler)
-- import           Control.Monad.IO.Class
import           Control.Monad
import           Data.Aeson
-- import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as C
import qualified Data.ByteString.Lazy         as BL
-- import           Data.Coerce
import           Data.Text                    as T
import qualified Data.Map.Strict              as M
import           Data.Maybe
import           HFlags
-- import           Llamabot.Types
import           Network.HTTP.Client          as NC
import qualified Network.HTTP.Client.TLS      as TLS
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.Wai                  as W
import           Network.Wai.Handler.Warp
-- import           Servant
import           Web.FormUrlEncoded



import           Llamabot
import           Slack



-------------------------- TESTY STUFF -----------------
import           Control.Concurrent
import           Control.Concurrent.STM
{-
type EventsListener = "event" 
                    :> ReqBody '[JSON] EventPayload
                    :> Post '[JSON] NoContent


data EventPayload = EventPayload
  { status :: Text
  }

instance ToJSON EventPayload where
  toJSON (EventPayload s) = String s


instance FromJSON EventPayload where
  parseJSON (String s) = return $ EventPayload s
  parseJSON o = error $ "you shouldn't send this thing to this endpoint " ++ show o



server :: Server EventsListener
server = event
  where event :: EventPayload -> Handler NoContent
        event EventPayload{..} = do 
          liftIO $ putStrLn $ "guess what event is ???? " ++ (show status)
          return NoContent

app :: Proxy EventsListener
app = Proxy
-}



defineFlag "token" ("" :: String) "Token for sending requests to slack"
$(return [])




------------------------------- APP LOGIC ---------------------------------


data IRForm = IRForm String deriving (Eq, Show)

instance FromForm IRForm where
  fromForm f = IRForm
    <$> parseUnique "payload" f


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
         
          when (hasLlamasAndTag ev) $ do
            let (llamaTotal, recipient) = parseLlamaPost $ text ev
            _ <- forkIO $ sendLlamaResponse contextT (channel ev) (user ev) recipient (ts ev) llamaTotal
            putStrLn $ "handler thread forked, returning 200"
          
          -- the 200 response
          responder $ responseLBS status200 [] BL.empty


defaultChannel :: Text
defaultChannel = "C08GV70G1GU" -- this is the #llamalog channel in LaunchLiveNow


sendLlamaResponse :: LlamaContextT -> Maybe Text -> Text -> Text -> Text -> Integer -> IO ()
sendLlamaResponse contextT mChannel sender recipient _ number = do -- thread is the missing one
  let channel = fromMaybe defaultChannel mChannel
  manager <- TLS.newTlsManager
  initRequest <- parseRequest "https://slack.com/api/chat.postMessage"
  -- let msgBlocks = 
  --      [ BlockText (TextBlock "plain_text" False "Hello!")
  --      , BlockElements
  --          [ ButtonBody (TextBlock "plain_text" False "CLICK ME!") "primary" "left button"
  --          , ButtonBody (TextBlock "plain_text" False "No, CLICK ME!!!") "danger" "right button"
  --          ]
  --      ]
  
  (recipientData, senderData, token) <- atomically $ do
    context <- readTVar contextT

  
    -- add the message
    let pm = ProcessedMessage 
          { pmMessage = "we don't save message body yet?"
          , pmSenderID = sender
          , pmRecipientID = recipient
          , pmChannelID = channel
          , pmTotal = number
          }
        messages = (lcMessages context) ++ [pm]
      
        -- update the user data
        recipientData' = case M.lookup recipient (lcUsers context) of
                          Nothing -> ActiveUser recipient 0 number number
                          Just (ActiveUser r st rw ra) -> ActiveUser r st (rw + number) (ra + number)
        senderData' = case M.lookup sender (lcUsers context) of
                          Nothing -> ActiveUser sender number 0 0
                          Just (ActiveUser r st rw ra) -> ActiveUser r (st + number) rw ra
 
        userMap' = M.insert recipient recipientData' (lcUsers context)
        userMap = M.insert sender senderData' userMap'

        finalContext = LlamaContext (lcToken context) (lcChannels context) messages userMap

    writeTVar contextT finalContext
    return (recipientData', senderData', lcToken context)
    

  --- QUICKLY STEAL CONTEXT, DELETE SOON
  finalContext <- atomically $ readTVar contextT
  putStrLn $ "\nCONTEXT UPDATE: \n" ++ show finalContext

  let msg = PostMessage
              channel
              (Just $ T.pack $ "Thanks " ++ (T.unpack $ "<@" <> sender <> ">") ++ "! You sent " ++ (T.unpack recipient) ++ "> " ++ (show number) ++ " llamas! You have sent " ++ (show $ auLlamasSentToday senderData) ++ " llamas today.  " ++ (T.unpack recipient) ++ "> has received " ++ (show $ auLlamasReceived recipientData) ++ " in their lifetime.")
              Nothing -- (Just thread)
              Nothing -- (Just msgBlocks)

      request = initRequest 
                { method = "POST"
                , NC.requestBody = RequestBodyLBS $ encode msg
                , NC.requestHeaders = 
                    [ (hAuthorization, (C.pack $ "Bearer " ++ (T.unpack token)))
                    , (hContentType, (C.pack "application/json"))
                    ]
                }

  response <- httpLbs request manager

  putStrLn $ "sent a response to llamas -- - the status code was " ++ (C.unpack $ BL.toStrict $ NC.responseBody response)



parseLlamaPost :: Maybe Text -> (Integer, Text)
parseLlamaPost Nothing = (0, "")
parseLlamaPost (Just t) = 
  let llamaTotal = toInteger $ Prelude.length $ T.breakOnAll ":llama:" t
      recipient =
        let prstr = snd $ Prelude.head $ T.breakOnAll "<@" t
        in fst $ Prelude.head $ T.breakOnAll ">" prstr
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



main :: IO ()
main = do
  _ <- $initHFlags "Lllamabot server v0.2"
 
  putStrLn $ "Welcome to Llamabot v0.3"
  putStrLn $ "OAUTH FLAG: " ++ (show $ flags_token)

  putStrLn "listening on 8081"

--
--
--  q <- atomically $ do 
--    qq <- newLlamaQueue 
--    writeTQueue qq "hiya"
--    writeTQueue qq "poopie"
--    return qq
 
--  tv <- atomically $ newDBTVar

--  tid <- forkIO $ do 
--    txt <- atomically $ readTQueue q
    
--     v <- atomically $ readTVar tv
--     atomically $ writeTVar tv (v ++ [42])
--    v1 <- atomically $ readTVar tv
--   atomically $ writeTVar tv (v1 ++ [40])
  

--  threadDelay 3000000
--   vals <- atomically $ flushTQueue q
--   t <- atomically $ readTVar tv
--  s <- atomically $ readTVar tv
--  putStrLn $ " is the tqueue empty?"  ++ show vals
--  putStrLn $ " is the tvar ?" ++ show t
--  putStrLn $ " is the tvar ?" ++ show s
--  killThread tid
  

  llamaT <- atomically $ initializeLlamaTVar (T.pack flags_token)

  run 8081 (app llamaT)

  -- run 8081 (serve app server)


