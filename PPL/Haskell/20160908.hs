-- @Author: YINAN
-- @Date:   2017-12-29 08:58:06
-- @Last Modified by:   YINAN
-- @Last Modified time: 2018-01-01 18:15:04

 
-- Consider the language of pictures L as in Exercise 1. 
-- Define the checkFig function, which takes a list of lists p and returns Just n, where n is the side of p, if p is a member of L;
-- Nothing otherwise. 
-- Write all the types of the defined functions.                   

checkRow :: Int -> [Int] -> Bool
checkRow n lst = check' n 1 lst where
    check' _ _ [] = True
    check' n p (x:xs) 
        | n == p = if x == 1 then check' n (p+1) xs else False
        | otherwise = if x == 0 then check' n (p+1) xs else False


checkSquare :: [[Int]] -> Bool
checkSquare lst = length lst  == length(head lst)



checkFig :: [[Int]] -> Maybe Int
checkFig lst = if not (checkSquare lst)
               then Nothing
               else  checkFig' lst 1 where
                    checkFig' [] n = Just $ n-1
                    checkFig' (x:xs) n = 
                        if checkRow n x then checkFig' xs (n+1) else Nothing