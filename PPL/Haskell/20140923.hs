-- @Author: YINAN
-- @Date:   2017-12-29 08:58:06
-- @Last Modified by:   YINAN
-- @Last Modified time: 2018-01-05 14:44:49


{-
2.1 Duplicates (5 pts)

Define rep in Haskell, without any limits of time complexity, and declaring its type.

-}

rep :: Eq a => [a] -> [a]
rep l = rep' l [] where
    rep' [] res = res
    rep' (x:xs) res 
        | (elem x xs) && (not (elem x res)) = rep' xs (res ++ [x])
        | otherwise = rep' xs res

{-
2.2 List comparison with duplicates (5 pts)

Define a predicate comprep for comparing lists, declaring its type. The predicate must accept another predicate (e.g. <=) and use it to compare the lists. The lists are compared counting the number of duplicated elements in them:
e.g. comprep((<=), [1,2,1,2], [0,0,1,0]) is false.

-}

comprep :: (Eq a, Eq b) => (Int -> Int -> Bool) -> [a] -> [b] -> Bool
comprep f l1 l2 = let n1 = (length . rep) l1
                      n2 = (length . rep) l2
                  in f n1 n2