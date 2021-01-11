module LogAnalysis where

import Log

-- Parse one line
-- > parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
-- > parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
-- > parseMessage "This is not in the right format"
-- >   == Unknown "This is not in the right format"
parseMessage :: String -> LogMessage
parseMessage string = case wordsList of
    ("I":x:xs) -> LogMessage Info (read x :: Int) (unwords xs)
    ("W":x:xs) -> LogMessage Warning (read x :: Int) (unwords xs)
    ("E":x:y:xs) -> LogMessage (Error (read x :: Int)) (read y :: Int) (unwords xs)
    _ -> Unknown string
    where wordsList = words string
-- When x is not a number, read x :: Int will raise an Exception

-- Parse whole log file
parse :: String -> [LogMessage]
parse xs = map parseMessage (lines xs)

-- Insert a new LogMessage into an existing MessageTree, producing a new one.
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert message Leaf = Node Leaf message Leaf 
insert log@(LogMessage _ x _) (Node leftTree origin@(LogMessage _ y _) rightTree)
    | x < y = Node (insert log leftTree) origin rightTree
    | x >= y = Node leftTree origin (insert log rightTree)

-- Build a complete MessageTree from a list of messages.
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf
-- build [] = Leaf
-- build (x:y) = insert x (build y)

-- In-order traversal of MessageTree
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node leftTree x rightTree) = inOrder leftTree ++ [x] ++ inOrder rightTree

-- takes an unsorted list of LogMessages
-- returns a list of the messages corresponding to any errors with a severity of 50 or greater, sorted by timestamp.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong logs = [string | (LogMessage (Error severity) _ string) <- sortedList, severity >= 50]
    where sortedList = inOrder (build logs)


-- We can also using recursion to construct the list
-- whatWentWrong` [] = []
-- whatWentWrong` [(LogMessage _ _ string)] = [string] 
-- whatWentWrong` [x:xs] = whatWentWrong x ++ whatWentWrong xs