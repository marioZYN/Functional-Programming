-- @Author: YINAN
-- @Date:   2017-12-29 08:58:06
-- @Last Modified by:   YINAN
-- @Last Modified time: 2018-01-06 21:56:32


{-
2 Haskell 

Define the function infixes, which takes a list g as input and returns the list of all infixes (i.e. non-empty contiguous sublists) of g.

For instance, infixes "ciao" is the list ["o","ao","iao","ciao","a","ia","cia","i","ci","c"] (remember that a string is a list of characters in Haskell).

-}

{-# OPTIONS_GHC -Wall #-}

comb :: String -> [String]
comb str = comb' str [] [] where
    comb' [] _ res = res
    comb' (x:xs) cur res = let new = cur ++ [x]
                           in comb' xs new (res ++ [new])

infixes :: String -> [String]
infixes [] = []
infixes str@(_:xs) = (comb str) ++ (infixes xs)