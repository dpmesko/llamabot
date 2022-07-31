{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TemplateHaskell    #-}

-- import           Control.Exception            hiding (Handler)
-- import           Control.Monad.IO.Class
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



--------------------------------- EVENTS API --------------------------------
data CallbackType = URLVerification | EventCallback deriving (Show, Eq)

data Authorization = Authorization
  { aEnterpriseId :: Maybe Text
  , aTeamId :: Text
  , aUserId :: Text
  , aIsBot :: Bool
  } deriving (Show, Eq)


data Event = Event
  { typ :: Text
  , text :: Maybe Text
  , eventTimestamp :: Text
  , user :: Text
  , ts :: Text
  , item :: Maybe Text
  } deriving (Show, Eq)


data Message =
    Handshake 
      { token :: Text
      , challenge :: Text
      , htyp :: CallbackType
      }
  | EventDetails
      { token :: Text
      , teamId :: Text
      , apiAppId :: Text
      , event :: Event
      , etyp :: CallbackType
      , authedUsers :: Maybe Text
      , authedTeams :: Maybe Text
      , authorizations :: [Authorization]
      , eventContext :: Text
      , eventId :: Text
      , eventTime :: Integer
      } deriving (Show, Eq)


instance FromJSON CallbackType where
  parseJSON (String str) = case str of
      "url_verification" -> return URLVerification
      "event_callback" -> return EventCallback
      x -> error $ "unknown callback type: " ++ show x
  parseJSON x = error $ "unknown callback type: " ++ show x


instance FromJSON Authorization where
  parseJSON (Object o) = do
    ei <- o .:? "enterprise_id"
    ti <- o .: "team_id"
    ui <- o .: "user_id"
    ib <- o .: "is_bot"
    return $ Authorization ei ti ui ib
  parseJSON x = error $ "unknown authorizations type: " ++ show x
     
instance FromJSON Event where
  parseJSON (Object o) = do
    tp <- o .: "type"
    tx <- o .:? "text"
    es <- o .: "event_ts"
    us <- o .: "user"
    ts <- o .: "ts"
    it <- o .:? "item"
    return $ Event tp tx es us ts it
  parseJSON x = error $ "unknown event type: " ++ show x

instance FromJSON Message where
  parseJSON (Object o) = do
    tp <- o .: "type"
    case tp of
      URLVerification -> do
        to <- o .: "token"
        ch <- o .: "challenge"
        return $ Handshake to ch tp
      EventCallback -> do
        to <- o .: "token"
        ti <- o .: "team_id"
        ai <- o .: "api_app_id"
        ev <- o .: "event"
        au <- o .:? "authed_users"
        at <- o .:? "authed_teams"
        as <- o .: "authorizations"
        ec <- o .: "event_context"
        ei <- o .: "event_id"
        et <- o .: "event_time"
        return $ EventDetails to ti ai ev tp au at as ec ei et
  parseJSON x = error $ "unknown message type " ++ show x



------------------------------ MESSAGE ENDPOINTS ------------------------------

data PostMessage = PostMessage
  { pmChannel :: Text
  , pmText :: Text
  } deriving (Eq, Show)


instance ToJSON PostMessage where
  toJSON (PostMessage c t) = object
    [ "channel" .= c
    , "text" .= t
    ]

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
        ev@(EventDetails _ _ _ _ _ _ _ _ _ _ _) -> do 
          putStrLn $ "got some event details: " ++ show ev
          let llamaTotal = countLlamas $ event ev
          putStrLn $ "  ======== NUMBER OF LLAMAS: " ++ (show llamaTotal)
          
          -- TODO: we should respond 200 before processing the llamas and sending the response, so
          --        we need to figure out whether to multithread or how to send the 200 from the app
          --        handler and then run extra IO code
          
          -- the message reply
         
          sendLlamaResponse "CL9G1JP6U" "john" "bob" llamaTotal
          -- the 200 response
          putStrLn $ "responding now"
          responder $ responseLBS status200 [] BL.empty



sendLlamaResponse :: Text -> Text -> Text -> Int -> IO ()
sendLlamaResponse channel sender receiver number = do
  manager <- TLS.newTlsManager
  initRequest <- parseRequest "https://slack.com/api/chat.postMessage"
  let msg = PostMessage
              channel
              (T.pack $ "Thanks " ++ (show sender) ++ "! You sent " ++ (show receiver) ++ " " ++ (show number) ++ " llamas!")


      request = initRequest 
                { method = "POST"
                , NC.requestBody = RequestBodyLBS $ encode msg
                , NC.requestHeaders = 
                    [ (hAuthorization, (C.pack $ "Bearer " ++ flags_token))
                    , (hContentType, (C.pack "application/json"))
                    ]
                }

  response <- httpLbs request manager
  putStrLn $ show response

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


