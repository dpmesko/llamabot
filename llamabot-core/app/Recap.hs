{-# LANGUAGE OverloadedStrings  #-}

import           Data.List
import           Data.Time
import           Data.Time.Calendar.WeekDate
-- import           Data.Text

import           Llamabot.Database

main :: IO ()
main = do
  conn <- dbConnect

  usersByTotalSentThisWeek <- sortUsersByTotalSentThisWeek conn Nothing
  usersByTotalReceivedThisWeek <- sortUsersByTotalReceivedThisWeek conn Nothing
 
  currentDay <- getCurrentTime >>= return . utctDay

  let bestSender = fst $ head usersByTotalSentThisWeek
      bestRecipient = fst $ head usersByTotalReceivedThisWeek
      (year, week, _) = toWeekDate currentDay
      firstDayOfWeek = fromWeekDate year week 1

  messagesFromTopSender <- 
    selectMessagesBySenderFromDate conn bestSender firstDayOfWeek Nothing
  messagesToTopRecipient <- 
    selectMessagesByRecipientFromDate conn bestRecipient firstDayOfWeek Nothing

  dbClose conn


  let sortByBodyLength (b1, _) (b2, _) 
        | b1 > b2 = GT
        | b2 > b1 = LT
        | otherwise = EQ
      longestSent = maximumBy sortByBodyLength messagesFromTopSender
      longestReceived = maximumBy sortByBodyLength messagesToTopRecipient

  putStrLn $ "users by total sent this week: " ++ show usersByTotalSentThisWeek
  putStrLn $ "users by total recv this week: " ++ show usersByTotalReceivedThisWeek
  putStrLn $ "top sender message: " ++ show longestSent
  putStrLn $ "top recipient message: " ++ show longestReceived
