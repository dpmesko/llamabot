{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE TypeOperators      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

-- import           Control.Exception            hiding (Handler)
-- import           Control.Monad.IO.Class
import           Data.Aeson
-- import qualified Data.ByteString              as B
import qualified Data.ByteString.Char8        as C
import qualified Data.ByteString.Lazy         as BL
import           Data.Text
import           Network.HTTP.Types.Header
import           Network.HTTP.Types.Status
import           Network.Wai
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



data Handshake = Handshake
  { token :: Text
  , challenge :: Text
  , typ :: Text
  } deriving (Show, Eq)

instance FromJSON Handshake where
  parseJSON (Object o) = do
    to <- o .: "token"
    ch <- o .: "challenge"
    tp <- o .: "type"
    return $ Handshake to ch tp
  parseJSON o = error $ "parseJSON for handshake received " ++ show o

app2 :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app2 req responder = do 
  putStrLn " fuck"
  body <- strictRequestBody req
  let c = maybe ("ERROR parsing") (challenge) (decode body)
  putStrLn $ "what is the request body?? " ++ (show body)
  putStrLn $ "what is C?? " ++ (show c)
  (responder $ responseLBS status200 [(hContentType, "text/plain")] (BL.fromStrict $ C.pack $ show c))


main :: IO ()
main = do
  putStrLn "listening on 8081"
  run 8081 app2
  -- run 8081 (serve app server)


