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
                                (Unknown _ ) -> mTree
				(LogMessage mt ts m) -> case mTree of
				       	     	     Leaf -> (Node Leaf logM Leaf)
						     (Node l lm r) -> if ts <= time then (Node (insert logM l) lm r) else (Node l lm (insert logM r))
						                      where time = getTime lm

getTime :: LogMessage -> TimeStamp
getTime (LogMessage _ ts _ ) = ts

build :: [LogMessage] -> MessageTree
build logM = case logM of 
                [m] -> insert m Leaf
                (m:ms) -> insert m $ build ms  


inOrder :: MessageTree -> [LogMessage]
inOrder mt = case mt of 
                (Node Leaf m Leaf) -> [m]
		(Node Leaf m r) -> [m] ++ inOrder r
		(Node l m Leaf) -> inOrder l ++ [m]
		(Node l m r) -> inOrder l ++ [m] ++ inOrder r


		
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logMs = case logMs of
                               ([m]) -> case m of
			                         (LogMessage (Error l) _ m) -> if l >= 50 then [m] else []
						 _ -> []
			       (m:ms) ->  case m of
			                         (LogMessage (Error l) _ m) -> if l >= 50 then m:whatWentWrong ms else whatWentWrong ms
						 _ -> whatWentWrong ms 

