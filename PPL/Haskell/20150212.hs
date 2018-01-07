-- @Author: YINAN
-- @Date:   2017-12-29 08:58:06
-- @Last Modified by:   YINAN
-- @Last Modified time: 2018-01-05 12:40:41


{-
2.1 Split (3 points)

Implement split in Haskell, noting that the Scheme version returns a vector: is there a more suitable type in Haskell for the job? If so, use it.

-}

split :: [a] -> Int -> ([a], [a]) 
split lst n = split' n [] lst where
    split' 0 l1 l2 = (l1, l2)
    split' n l1 (x:xs) = split' (n-1) (l1 ++ [x]) xs

{-
2.2 Factors (7 points)

Implement 3-factors in Haskell, noting that the Scheme version returns a list of lists: is there a more suitable type in Haskell for the job? If so, use it.

-}
threeFactors :: [a] -> [([a], [a], [a])]
threeFactors lst = threeFactors' lst 1 [] 

threeFactors' lst i res 
    | i > (length lst - 2) = res
    | otherwise = let 
                    (first, rest) = split lst i
                    cur = inner rest first 1 [] where
                        inner lst first n res
                            | n > (length lst - 1) = res
                            | otherwise = let (second, third) = split lst n
                                              r = (first, second, third)
                                          in inner lst first (n+1) (res ++ [r])
                in threeFactors' lst (i+1) (res ++ cur)
