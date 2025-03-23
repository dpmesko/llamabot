{-# LANGUAGE OverloadedStrings  #-}


module Llamabot.Commands (
  handleMeCommand,
  handleLeaderboardCommand,
  handleInvalidCommand
  ) where


import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Char8        as C
import           Data.Text                    as T

import           Network.HTTP.Client          as NC
import qualified Network.HTTP.Client.TLS      as TLS
import           Network.HTTP.Types.Header


import           Slack
import           Llamabot.Database
import           Llamabot.Message


handleMeCommand :: SlashCommand -> Text -> IO ()
handleMeCommand command tkn = do

  dbConn <- dbConnect
  mUser <- selectUserById dbConn $ scUserId command
  theUser <- case mUser of
   Just user' -> return user'
   Nothing -> do
     let newUser = DBUser { userId                 = scUserId command
                          , llamaDailyAllotment    = defaultDailyAllotment
                          , llamasReceivedThisWeek = 0
                          , llamasSentThisWeek     = 0 
                          , llamasReceived         = 0
                          , llamasSent             = 0
                          }
     insertUser dbConn newUser
     return newUser
  
  dbClose dbConn

  let msgBlocks = Blocks "/llamabot me" 
        [ BlockText (TextBlock "plain_text" "Your Llama Totals")
        , BlockFields
            [ Field "mrkdwn" $ T.pack $ "*Daily Allotment Remaining*\n" ++ (show $ llamaDailyAllotment theUser)
            , Field "mrkdwn" $ T.pack $ "*Sent This Week*\n" ++ (show $ llamasSentThisWeek theUser)
            , Field "mrkdwn" $ T.pack $ "*Received This Week*\n" ++ (show $ llamasReceivedThisWeek theUser)
            , Field "mrkdwn" $ T.pack $ "*Sent*\n" ++ (show $ llamasSent theUser)
            , Field "mrkdwn" $ T.pack $ "*Received*\n" ++ (show $ llamasReceived theUser)
            ]
        ]
   
  manager <- TLS.newTlsManager
  initRequest <- parseRequest $ T.unpack $ scResponseUrl command
  let request = initRequest
                  { NC.method = "POST"
                  , NC.requestBody = RequestBodyLBS $ encode msgBlocks
                  , NC.requestHeaders =
                    [ (hAuthorization, (C.pack $ "Bearer " ++ (T.unpack tkn)))
                    , (hContentType, (C.pack "application/json"))
                    ]
                  }
  void $ httpLbs request manager





handleLeaderboardCommand :: SlashCommand -> Text -> IO ()
handleLeaderboardCommand command tkn = do
  dbConn <- dbConnect
  sentUsers <- sortUsersByTotalSent dbConn Nothing -- Just limit
  receivedUsers <- sortUsersByTotalReceived dbConn Nothing -- Just limit
  dbClose dbConn

  let createUserBlock :: (Text, Integer) -> RichTextElement
      createUserBlock (uid, total) = RichTextSection 
        [ RichTextText $ T.pack $ show total
        , RichTextText " --> "
        , RichTextUser uid
        ]
                                      
      sentUserBlocks = Prelude.map createUserBlock sentUsers 
      receivedUserBlocks = Prelude.map createUserBlock receivedUsers   
      
      msgBlocks = Blocks "/llamabot leader" $ 
        [ BlockText (TextBlock "plain_text" "Llama Leaderboards\n")
        , BlockFields [Field "mrkdwn" "*Llamas Sent*\n"]
        , BlockRichText [RichTextList Ordered sentUserBlocks]
        , BlockFields [Field "mrkdwn" "*Llamas Received*\n"]
        , BlockRichText [RichTextList Ordered receivedUserBlocks]
        ]

  manager <- TLS.newTlsManager
  initRequest <- parseRequest $ T.unpack $ scResponseUrl command
  let request = initRequest
                  { NC.method = "POST"
                  , NC.requestBody = RequestBodyLBS $ encode msgBlocks
                  , NC.requestHeaders =
                    [ (hAuthorization, (C.pack $ "Bearer " ++ (T.unpack tkn)))
                    , (hContentType, (C.pack "application/json"))
                    ]
                  }
  void $ httpLbs request manager


handleInvalidCommand :: SlashCommand -> Text -> IO ()
handleInvalidCommand command tkn = do
  let msgBlocks = Blocks "/llamabot" 
        [ BlockText (TextBlock "plain_text" "Llamabot Slash Commands\n")
        , BlockFields [Field "plain_text" "See the list of commands below"]
        , BlockRichText [RichTextList Bullet 
            [ RichTextSection [RichTextText "/llamabot me --> view your llama totals"]
            , RichTextSection [RichTextText "/llamabot leader --> view the llama leaderboard"]
            ]]
         ]

  manager <- TLS.newTlsManager
  initRequest <- parseRequest $ T.unpack $ scResponseUrl command
  let request = initRequest
                  { NC.method = "POST"
                  , NC.requestBody = RequestBodyLBS $ encode msgBlocks
                  , NC.requestHeaders =
                    [ (hAuthorization, (C.pack $ "Bearer " ++ (T.unpack tkn)))
                    , (hContentType, (C.pack "application/json"))
                    ]
                  }
  void $ httpLbs request manager

