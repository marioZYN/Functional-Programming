-- @Author: YINAN
-- @Date:   2018-01-10 06:39:26
-- @Last Modified by:   YINAN
-- @Last Modified time: 2018-01-12 08:50:15

-- sorry guys, but I don't like the idea of making the code as short as possible. Lucky for me, I don't follow the course.

-- first exercise

module Golf where
import Data.List

divi :: [a] -> Int -> [a]
divi lst n 
    | n > length lst = []
    | otherwise = let (first, second) = splitAt n lst
                  in [last first] ++ (divi second n)

skips :: [a] -> [[a]]
skips lst = reverse $ skips' (length lst) lst where
    skips' 0 lst = [] 
    skips' n lst  = [divi lst n] ++ skips' (n-1) lst


-- second exercise
localMaxima :: [Integer] -> [Integer]
localMaxima (x1:x2:x3:[]) = if x1 < x2 && x2 > x3 then [x2] else []
localMaxima l@(x1:x2:x3:_) = if x1 < x2 && x2 > x3 then [x2] ++ (localMaxima (tail l)) else localMaxima (tail l)
localMaxima _ = []

-- third exercise
dispN :: [Integer] -> String
dispN lst = dispN' lst 0 "" where
    dispN' _ 10 res = res ++ "\n"
    dispN' [] 0 _ = []
    dispN' [] cur res = dispN' [] (cur+1) (res ++ " ")
    dispN' l@(x:xs) cur res
        | cur == x = dispN' xs (cur+1) (res ++ "*")
        | otherwise = dispN' l (cur+1) (res ++ " ")

split :: [Integer] -> [Integer] -> [Integer] -> ([Integer], [Integer])
split [] l1 l2 = (l1, l2)
split (x:xs) l1 l2 
    | x `elem` xs = split xs l1 (l2 ++ [x])
    | otherwise = split xs (l1 ++ [x]) l2

histogram :: [Integer] -> String
histogram lst = histogram' lst "" where
    histogram' [] res = res ++ "==========\n" ++ "0123456789\n" 
    histogram' lst res = let (l1, l2) = split (sort lst) [] []
                         in histogram' l2 ((dispN l1) ++ res)
    

