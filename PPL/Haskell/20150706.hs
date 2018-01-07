-- @Author: YINAN
-- @Date:   2017-12-29 08:58:06
-- @Last Modified by:   YINAN
-- @Last Modified time: 2018-01-05 11:12:24


{-

2.1 Split (5 points)

Assume this is a possible Haskell variant of producer in Exercise 1, in which the closures’ states are made explicit.

producer ag1 ag2 = prod’ ag1 [] ag2 0 1
    where
        prod’ ag1 st1 ag2 st2 i | i >= 10 = (st2:st1)
        prod’ ag1 st1 ag2 st2 i | odd  i = prod’ ag1 (ag1 i st1) ag2 st2 (i+1)
        prod’ ag1 st1 ag2 st2 i | even i = prod’ ag1 st1 ag2 (ag2 i st2) (i+1)

1. Define two suitable functions for producer’s two arguments, such that its call returns [20,9,7,5,3,1].
2. Write the type of prod’, assuming that all the numbers have type Int.

-}


producer ag1 ag2 = prod' ag1 [] ag2 0 1
    where
        prod' ag1 st1 ag2 st2 i | i >= 10 = (st2:st1)
        prod' ag1 st1 ag2 st2 i | odd  i = prod' ag1 (ag1 i st1) ag2 st2 (i+1)
        prod' ag1 st1 ag2 st2 i | even i = prod' ag1 st1 ag2 (ag2 i st2) (i+1)

ag1 :: Int -> [Int] -> [Int]
ag1 v st = [v] ++ st

ag2 :: Int -> Int -> Int  
ag2 v st = v + st

{-

2.2 Duo-fold (6 points)

Define an higher-order function called duofold, which takes two binary functions f and g, a starting value t and a (finite) list [e1, e2, . . .], and returns . . . f(g(f(t, e1), e2), e3), .... Please, write also its type.
Example: duofold (+) (-) 0 [1,2,3,4] returns −2 (i.e. 0 + 1 − 2 + 3 − 4).

-}

duofold :: (a -> b -> a) -> (a -> b -> a) -> a -> [b] -> a
duofold f g t l = duofold' f g t l 0  where
  duofold' _ _ t [] _  = t
  duofold' f g t (x:xs) c  
    | even c = duofold' f g (f t x) xs (c+1)
    | otherwise = duofold' f g (g t x) xs (c+1)







