{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE TemplateHaskell    #-}


import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans.Class
import qualified Data.ByteString.Lazy         as BL
import           Data.Text                    as T

import           Network.HTTP.Types.Status
import           Network.Wai                  as W
import           Network.Wai.Handler.Warp

import           HFlags

import           Llamabot
import           Slack


defineFlag "token" ("" :: String) "Token for sending requests to slack"
defineFlag "port" (8082:: Int) "Port number on which the app server will listen"
$(return [])



app :: Text -> W.Request -> (W.Response -> IO ResponseReceived) -> IO ResponseReceived
app tkn req responder = do 
  form <- strictRequestBody req
  runLoggingT $ do
    let eCommand = decodeSlashCommand form
    case eCommand of
      Left err -> $logError $ T.pack $ "Unable to Parse Slash Command Body: " ++ show err
      Right command -> do
        let commandType = scText command
        $logInfo $ T.pack $ "Received Slash Command: " ++ show commandType
      
        if commandType == "me" 
          then void $ lift $ forkIO $ handleMeCommand command tkn
        else if commandType == "leader" 
          then void $ lift $ forkIO $  handleLeaderboardCommand command tkn
        else void $ lift $ forkIO $ handleInvalidCommand command tkn
          
  -- the 200 response
  responder $ responseLBS status200 [] BL.empty


main :: IO ()
main = do
  _ <- $initHFlags "Lllamabot Slash Command Service"
 
  putStrLn "\n"
  putStrLn $ "The Llamabot Slash Command Service"
  putStrLn "\n"
  putStrLn $ "OAUTH FLAG: " ++ (show $ flags_token)

  putStrLn $ "-- checking MySQL database connection (credentials hardcoded for now)"
  
  conn <- dbConnect
  dbClose conn
  putStrLn $ "-- database connection secured"

  putStrLn "\n"
  putStrLn "starting server"
  putStrLn $ "listening on port " ++ (show flags_port) ++ "/n"

  run flags_port (app $ T.pack flags_token)



