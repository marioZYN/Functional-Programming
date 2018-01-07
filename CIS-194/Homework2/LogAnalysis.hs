-- @Author: YINAN
-- @Date:   2018-01-03 20:24:29
-- @Last Modified by:   YINAN
-- @Last Modified time: 2018-01-07 17:02:02

{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

parseMessage :: String -> LogMessage
parseMessage s = parseMessage' (words s) where
    parseMessage' l@(x1:x2:x3:xs)
        | x1 == "I" = LogMessage Info (read x2 :: Int) (unwords (x3:xs))
        | x1 == "W" = LogMessage Warning (read x2:: Int) (unwords (x3:xs))
        | x1 == "E" = LogMessage (Error (read x2 :: Int)) (read x3 :: Int) (unwords xs)
        | otherwise = Unknown (unwords l)
    parseMessage' l = Unknown (unwords l)

parse :: String -> [LogMessage]
parse whole = parse' (lines whole) where
    parse' [] = []
    parse' (l:ls) = parseMessage l : parse' ls

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert l Leaf = Node Leaf l Leaf
insert m1@(LogMessage _ ts _) (Node l m2@(LogMessage _ ts2 _) r)
    | ts < ts2 = Node (insert m1 l) m2 r
    | otherwise = Node l m2 (insert m1 r)
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build lst = build' lst Leaf where
    build' [] res = res
    build' (x:xs) res = build' xs (insert x res)

inOrder :: MessageTree -> [LogMessage]
inOrder tree = inOrder' tree where
    inOrder' Leaf = []
    inOrder' (Node l msglog r) = (inOrder l) ++ [msglog] ++ (inOrder r) 

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong unOrdered = let ordered = (inOrder . build) unOrdered
                          in whatWentWrong' ordered [] where
                                whatWentWrong' [] res = res
                                whatWentWrong' ((LogMessage (Error v) _ str):xs) res = if v >= 50 then whatWentWrong' xs (res ++ [str]) else whatWentWrong' xs res
                                whatWentWrong' (_:xs) res = whatWentWrong' xs res
