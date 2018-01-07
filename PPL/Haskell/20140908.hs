-- @Author: YINAN
-- @Date:   2017-12-29 08:58:06
-- @Last Modified by:   YINAN
-- @Last Modified time: 2018-01-05 18:52:03


{-
2 Haskell

2.1 Part I (8 pts)
Translate every procedure of the Scheme part into Haskell, assuming that the list of lists contains either Strings or Ints and defining suitable data structures, if needed.
Note:maxinHaskellhastypeOrd a => a -> a -> a.

2.2 Part II (5 pts)
Declare all the types of the functions defined in Part I.
-}

multiply_apply :: (a -> a) -> Int -> a -> a
multiply_apply _ 0 x = x
multiply_apply f n x = multiply_apply f (n-1) (f x)

position_of_max :: Ord a => [a] -> Int
position_of_max lst = position_of_max' lst 0 (foldr max (head lst) lst) where
    position_of_max' [] res _ = res
    position_of_max (x:xs) res mv 
        | x == mv = res
        | otherwise = position_of_max' xs (res+1) mv

data StrNum = S String | N Int deriving (Show, Eq)

norm :: StrNum -> Int
norm (S x) = length x
norm (N x) = x


max_of_the_longest :: [[StrNum]] -> Int
max_of_the_longest lol = let l1 = map length lol
                             pos = position_of_max l1
                             e = lol !! pos
                             l2 = fmap norm e
                         in foldr max 0 l2

