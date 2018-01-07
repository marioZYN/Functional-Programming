-- @Author: YINAN
-- @Date:   2017-12-29 08:58:06
-- @Last Modified by:   marioZhou
-- @Last Modified time: 2018-01-05 09:39:24


{-
2.1 Lists as instances of Num (6 points)
Make lists of numbers a instances of the class Num. If the two lists have different length, you must assume that the missing elements of the shorter are all 0.
E.g. [1,2,3] * [2,-1] should be [2,-2,0].
(Remember that you need to define methods for +, -, *, abs, signum, and fromInteger.)

-}

addZero :: (Num a) => [a] -> Int -> [a]
addZero lst l
    | length lst < l = addZero (lst ++ [0]) l
    | length lst == l = lst
    | otherwise = lst

instance Num a => Num [a] where
    l1 + l2 = let l1' = addZero l1 (length l2)
                  l2' = addZero l2 (length l1)
              in [x+y | x <- l1', y <- l2']

    l1 - l2 = let l1' = addZero l1 (length l2)
                  l2' = addZero l2 (length l1)
              in [x-y | x <- l1', y <- l2']

    l1 * l2 = let l1' = addZero l1 (length l2)
                  l2' = addZero l2 (length l1)
              in [x*y | x <- l1', y <- l2']

    abs l = [abs(x)| x <- l]
    signum l = [signum x | x <- l]
    fromInteger x = [fromInteger x]

{-
2.2 List of lists of lists... (3 points)

Define a recursive data structure of type TT which can be used to represent lists of Int of any depth (e.g. in Scheme ’(1 2 (3 9) ((1) -7))).

-}

data TT = VV Int | LL [TT] deriving (Show, Eq)

{-
2.3 Lile (3 points)

Define a predicate lile, which, given a TT value, check if it contains its own length.
E.g., using a Scheme-like notation (lile ’(2 1)) holds, while (lile ’(1 2 1)) does not.

-}

len (VV _) = 0
len(LL x) = length x

member x (LL y) = (VV x) `elem` y

lile :: TT -> Bool
lile x = len x `member` x

{-
2.4 Lileg (5 points)

Define a version of the predicate lile, called lileg, which takes a value of type TT, and check if all the lists in it contain their length.
E.g., using a Scheme-like notation: (lileg ’(2 (2 (1)) 3)) must hold.

-}

iflc (LL []) = True
iflc (LL (VV x):xs) = iflc (LL xs)
iflc (LL (x:xs)) = lileg x && iflc (LL xs)

lileg x = lile x && iflc x












