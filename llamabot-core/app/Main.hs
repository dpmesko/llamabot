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
         
          when (hasLlamasAndTag ev) $ do
            let (llamaTotal, recipient) = parseLlamaPost $ text ev
            _ <- forkIO $ handleLlamaResponse contextT (channel ev) (user ev) recipient (ts ev) llamaTotal
            putStrLn $ "handler thread forked, returning 200 OK"
          
          -- the 200 response
          responder $ responseLBS status200 [] BL.empty

 


defaultChannel :: Text
defaultChannel = "C08GV70G1GU" -- this is the #llamalog channel in LaunchLiveNow

  
handleLlamaResponse :: LlamaContextT -> Maybe Text -> Text -> Text -> Text -> Integer -> IO ()
handleLlamaResponse contextT mChannel sender recipient thread number = do

  currentDay <- getCurrentTime >>= return . utctDay
  
  let channel = fromMaybe defaultChannel mChannel
      llamaMessage = LlamaMessage { lmChannelId = channel
                                  , lmSender = sender
                                  , lmRecipient = recipient
                                  , lmTotal = number
                                  , lmMsgBody = "TODO TODO TODO lol parsingNeedsToBeDiff"
                                  , lmTs = thread
                                  , lmDate = currentDay
                                  }
 
  putStrLn $ "llama message parsed ----- " ++ show llamaMessage
  processResponse <- processLlamaMessage llamaMessage
  putStrLn $ show processResponse
  manager <- TLS.newTlsManager
  initRequest <- parseRequest "https://slack.com/api/reactions.add"
  -- let msgBlocks = 
  --      [ BlockText (TextBlock "plain_text" False "Hello!")
  --      , BlockElements
  --          [ ButtonBody (TextBlock "plain_text" False "CLICK ME!") "primary" "left button"
  --          , ButtonBody (TextBlock "plain_text" False "No, CLICK ME!!!") "danger" "right button"
  --          ]
  --      ]

  llamaContext <- atomically $ readTVar contextT

  let reaction = PostReaction channel "poop" thread



--  let msg = PostMessage
--              channel
--              (Just $ T.pack $ "Thanks " ++ (T.unpack $ "<@" <> sender <> ">") ++ "! You sent " ++ (T.unpack recipient) ++ "> " ++ (show number) ++ " llamas! You have sent " ++ (show $ auLlamasSentToday senderData) ++ " llamas today.  " ++ (T.unpack recipient) ++ "> has received " ++ (show $ auLlamasReceived recipientData) ++ " in their lifetime.")
--              Nothing -- (Just thread)
--              Nothing -- (Just msgBlocks)

      request = initRequest 
                { method = "POST"
                , NC.requestBody = RequestBodyLBS $ encode reaction
                , NC.requestHeaders = 
                    [ (hAuthorization, (C.pack $ "Bearer " ++ (T.unpack $ lcToken llamaContext)))
                    , (hContentType, (C.pack "application/json"))
                    ]
                }

  response <- httpLbs request manager

  putStrLn $ "sent a response to llamas -- - the status code was " ++ (C.unpack $ BL.toStrict $ NC.responseBody response)






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
  putStrLn "listening on 8081"


  run 8081 (app llamaT)



