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
-- import           Data.Maybe
import           Data.Text                    as T
import           HFlags
import           Network.HTTP.Client          as NC
import qualified Network.HTTP.Client.TLS      as TLS
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.Wai                  as W
import           Network.Wai.Handler.Warp
-- import           Servant


import           Slack

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
app :: W.Request -> (W.Response -> IO ResponseReceived) -> IO ResponseReceived
app req responder = do 
  body <- strictRequestBody req
  let msg' = eitherDecode body
  case msg' of
    Left str -> error str
    Right msg -> do
      case msg of
        Handshake _ ch _ -> do 
          putStrLn $ "got a handshake, responding with the challenge: " ++ show ch
          responder $ responseLBS status200 [(hContentType, "text/plain")] (BL.fromStrict $ C.pack $ show ch)
        ed@(EventDetails _ _ _ ev _ _ _ _ _ _ _) -> do 
          putStrLn $ "got some event details: " ++ show ed
          
          -- TODO: we should respond 200 before processing the llamas and sending the response, so
          --        we need to figure out whether to multithread or how to send the 200 from the app
          --        handler and then run extra IO code
          
          -- the message reply
          when (hasLlamasAndTag ev) $ do
            let (llamaTotal, recipient) = parseLlamaPost $ text ev
            sendLlamaResponse "CL9G1JP6U" (user ev) recipient (ts ev) llamaTotal
          
          -- the 200 response
          responder $ responseLBS status200 [] BL.empty

sendLlamaResponse :: Text -> Text -> Text -> Text -> Int -> IO ()
-- sendLlamaResponse channel sender receiver thread number = do
sendLlamaResponse channel _ _ _ _ = do
  manager <- TLS.newTlsManager
  initRequest <- parseRequest "https://slack.com/api/chat.postMessage"
  let msgBlocks = 
        [ Block Header (BlockText (TextBlock "plain_text" False "Hello!"))
        , Block Actions (BlockElements [Element Button (ButtonBody (TextBlock "plain_text" False "CLICK ME!") "primary" "12345")])
        ]
      msg = PostMessage
              channel
              Nothing -- (T.pack $ "Thanks " ++ (T.unpack $ "<@" <> sender <> ">") ++ "! You sent " ++ (T.unpack receiver) ++ "> " ++ (show number) ++ " llamas!")
              Nothing -- (Just thread)
              (Just msgBlocks)

      request = initRequest 
                { method = "POST"
                , NC.requestBody = RequestBodyLBS $ encode msg
                , NC.requestHeaders = 
                    [ (hAuthorization, (C.pack $ "Bearer " ++ flags_token))
                    , (hContentType, (C.pack "application/json"))
                    ]
                }

  response <- httpLbs request manager
  putStrLn $ "sent a response to llamas - the status code was " ++ (C.unpack $ BL.toStrict $ NC.responseBody response)



parseLlamaPost :: Maybe Text -> (Int, Text)
parseLlamaPost Nothing = (0, "")
parseLlamaPost (Just t) = 
  let llamaTotal = Prelude.length $ T.breakOnAll ":llama:" t
      recipient =
        let prstr = snd $ Prelude.head $ T.breakOnAll "<@" t
        in fst $ Prelude.head $ T.breakOnAll ">" prstr
  in (llamaTotal, recipient)

hasLlamasAndTag :: Event -> Bool
hasLlamasAndTag ev = (countLlamas ev > 0) && (countTags ev == 1)

countTags :: Event -> Int
countTags Event{..} = 
  case text of
    Nothing -> 0
    Just t -> Prelude.length $ T.breakOnAll "<@" t


countLlamas :: Event -> Int
countLlamas Event{..} =
  case text of
    Nothing -> 0
    Just t -> Prelude.length $ T.breakOnAll ":llama:" t






main :: IO ()
main = do
  _ <- $initHFlags "Lllamabot server v0.1"
  
  putStrLn $ "OAUTH FLAG: " ++ (show $ flags_token)

  putStrLn "listening on 8081"
  run 8081 app
  -- run 8081 (serve app server)


