-- @Author: YINAN
-- @Date:   2017-12-29 08:58:06
-- @Last Modified by:   YINAN
-- @Last Modified time: 2018-01-01 12:14:06

 
-- Given a list of lists, define a function transpose that returns a list containing: a list of all the first elements, then a list of all the second elements, and so on. Lists can be assumed non empty, but can be of different lengths. Write all the types of the defined functions.
-- E.g. transpose [[1,2],[3],[4,5,6]] is the list [[1,3,4],[2,5],[6]].

-- transpose :: [[a]] -> [[a]]
-- transpose lol = transpose' lol [] where
--     transpose' lol res = let heads = [head x | x <- lol, (length x > 0)]
--                              tails = [tail x | x <- lol, (length x > 0)]
--                              sums = foldl (\x y -> (length y) + x ) 0 lol
--                          in if sums == 0 then reverse res else transpose' tails (heads:res)
--                          

transpose :: [[a]] -> [[a]] 
transpose [] = []
transpose ls = let hs = map head ls
                   ts = filter (not . null) $ map tail ls 
               in hs : transpose ts