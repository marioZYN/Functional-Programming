-- @Author: YINAN
-- @Date:   2017-12-29 08:58:06
-- @Last Modified by:   YINAN
-- @Last Modified time: 2018-01-06 12:26:59


{-
2 Haskell

Consider a variant of the problem seen in Exercise 1: an unsorted list can contain list of elements of some type, integer numbers, or the special value End.
E.g. (in a pseudo-Haskell syntax) [3, [6, 6, 1], 4, [1, 2], -2, End, 9].

2.1 Data structures (5 pts)
Define the data structure for the unsorted list, and Demuxed, analogous to the structure introduced in Exer- cise 1 (i.e. with two fields, one containing an integer value, the sum of the integer elements found, and a list of all the found lists).

2.2 Demux (6 pts)
Define the demux function, that takes an unsorted list l and builds up a demuxed data structure containing the processed data of l (only that before End, if present). demux must be strict (i.e. non lazy).

-}

data UnSorted a = I Integer | L [a] | End deriving Show

data Demuxed a = Demuxed Integer [a] deriving Show

demux :: [UnSorted a] -> Demuxed a
demux lst = demux' lst 0 [] where
    demux' [] num vec = Demuxed num vec
    demux' ((I x):xs) num vec = let n = x + num
                                in seq n (demux' xs n vec)
    demux' ((L x):xs) num vec = let v = vec ++ x
                                in seq v (demux' xs num v)
    demux ((End):xs) num vec = Demuxed num vec