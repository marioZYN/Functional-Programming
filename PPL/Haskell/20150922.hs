-- @Author: YINAN
-- @Date:   2017-12-29 08:58:06
-- @Last Modified by:   marioZhou
-- @Last Modified time: 2018-01-04 11:24:52

data Bilist a = Bilist [a] [a] deriving (Show, Eq)

bilist_ref :: Bilist a -> Int -> (a, a)
bilist_ref (Bilist l1 l2) 0 = (head l1, head l2)
bilist_ref (Bilist (x1:xs1) (x2:xs2)) n = bilist_ref (Bilist xs1 xs2) (n-1)

oddlists :: [Int] -> [Int]
oddlists lst = odd' lst 0 [] where
    odd' [] _ res = res
    odd' (x:xs) c res 
        | odd c = odd' xs (c+1) (res ++ [x])   
        | otherwise = odd' xs (c+1) res

evenlists :: [Int] -> [Int]
evenlists lst = even' lst 0 [] where
    even' [] _ res = res
    even' (x:xs) c res
        | even c = even' xs (c+1) (res ++ [x])
        | otherwise = even' xs (c+1) res

oddeven :: [Int] -> Bilist Int
oddeven lst = Bilist (evenlists lst) (oddlists lst)

inv_oddeven :: Bilist Int -> [Int]
inv_oddeven (Bilist [] []) = []
inv_oddeven (Bilist (x1:xs1) (x2:xs2)) = [x1] ++ [x2] ++ (inv_oddeven (Bilist xs1 xs2))

sum_bilist_ref :: Bilist Int -> Int -> Int
sum_bilist_ref blist pos = let (x1,x2) = bilist_ref blist pos
                           in (x1+x2)

len_bilist :: Bilist a -> Int
len_bilist (Bilist l1 _) = length l1

bilist_max :: Bilist Int -> Int
bilist_max b = bilist_max' b 0 0 where
    bilist_max' b pos res 
        | pos == (len_bilist b) = res
        | otherwise = if (sum_bilist_ref b pos) >= (sum_bilist_ref b res)
                        then bilist_max' b (pos+1) pos
                        else bilist_max' b (pos+1) res











