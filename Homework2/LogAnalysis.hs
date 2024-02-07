{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

-- Parses an individual line from our logs
-- > parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
-- > parseMessage "I 29 la la la"     == LogMessage Info 29 "la la la"
-- > parseMessage "This is not in the right format"
-- > == Unknown "This is not in the right format"
parseMessage :: String -> LogMessage
parseMessage str =
    case words str of
        ("I" : timestamp : msg) ->
            LogMessage
                Info
                (read timestamp)
                (unwords msg)
        ("W" : timestamp : msg) ->
            LogMessage
                Warning
                (read timestamp)
                (unwords msg)
        ("E" : severity : timestamp : msg) ->
            LogMessage
                ( Error
                    (read severity)
                )
                (read timestamp)
                (unwords msg)
        _ -> Unknown str

-- Parses the whole log file and returns its contents as a list of LogMessages
parse :: String -> [LogMessage]
parse logContent = map parseMessage (lines logContent)

-- Inserts a new log into the MessageTree
-- We consider that the MessageTree is already sorted and inserting
-- new logs should keep it sorted
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) msgTree = msgTree -- Ignores Unknown messages
insert logMsg Leaf = Node Leaf logMsg Leaf -- Inserts on an empty tree
insert
    logMsg@(LogMessage _ timestamp _)
    (Node left nodeLogMsg@(LogMessage _ nodeTimeStamp _) right)
        | timestamp < nodeTimeStamp = Node (insert logMsg left) nodeLogMsg right -- Inserts to the left
        | timestamp > nodeTimeStamp = Node left nodeLogMsg (insert logMsg right) -- Inserts to the right
insert _ tree = tree -- Base case to treat unexpected cases

-- Builds up a MessageTree containing the messages in the list
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- Traverse our MessageTree in order (by timestamp) and returns the sorted 
-- list
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logMsg right) = inOrder left ++ [logMsg] ++ inOrder right

-- Traverses the tree in preorder fashion 
preOrder :: MessageTree -> [LogMessage]
preOrder Leaf = []
preOrder (Node left logMsg right) = logMsg : preOrder left ++ preOrder right

-- Postorder traversal
postOrder :: MessageTree -> [LogMessage]
postOrder Leaf = []
postOrder (Node left logMsg right) = postOrder left ++ postOrder right ++ [logMsg]

-- Given a list of LogMessages, returns all errors in the log with 
-- severity >= 50 sorted by timestamp (ascending)
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs = 
  let sortedLogs = inOrder (build logs)
      filteredLogs = filter isRelevantError sortedLogs
  in map getMessage filteredLogs

-- Helper to look up for relevant errors (severity >= 50)
isRelevantError :: LogMessage -> Bool
isRelevantError (LogMessage (Error severity) _ _) = severity >= 50 
isRelevantError _ = False

-- Helper to return the message of the LogMessage
getMessage :: LogMessage -> String
getMessage (LogMessage _ _ message) = message 
getMessage (Unknown _) = "" -- empty for unknown message 
