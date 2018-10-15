--{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where 
import Log
import Text.Read
parseMessage :: String -> LogMessage
parseMessage s
                | elem errorCode "EWI" = LogMessage messageType timeStamp message
                | otherwise = Unknown "This is not in the right format"
                where errorCode = head s
                      errorLevel = getStr s 1
                      messageType = getMessage $ findMessageType errorCode $ read $ errorLevel
                      timeStamp = if errorCode /= 'E' then read $ getStr s 1 else read $ getStr s 2
                      message = if errorCode /= 'E' then unwords $ drop 2 $ words s else unwords $ drop 3 $ words s 
          
getStr :: String -> Int -> String
getStr s i = head $ drop i $ words s
getMessage :: Maybe MessageType -> MessageType
getMessage (Just mt) = mt 
findMessageType :: Char -> Int -> Maybe MessageType
findMessageType mType mLevel
                   | mType == 'E' = Just (Error mLevel)
                   | mType == 'I' = Just Info
                   | mType == 'W' = Just Warning
                   | otherwise    = Nothing

parse :: String -> [LogMessage]
parse s = map parseMessage $ lines s



insert :: LogMessage -> MessageTree -> MessageTree
insert logM mTree  = case logM of 
                        (Unknown m)-> mTree
                        _ -> (Node mTree logM mTree)


--build :: [LogMessage] -> MessageTree
--build 
