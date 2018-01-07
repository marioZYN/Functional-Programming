-- @Author: YINAN
-- @Date:   2017-12-29 08:58:06
-- @Last Modified by:   YINAN
-- @Last Modified time: 2018-01-04 16:46:56

{- |
2.1 Class definition (3 points)

Define a class called Blup, for a generic type T having two parameters x and y, providing two operations called fisto and fosto. fisto takes a value belonging to T and returns a value of type Maybe x, while fosto takes a value belonging to T and returns a value of type Maybe y.

2.2 Instance I (4 points)

Define the sum type Blargh with two parameters of types a and b. It has three data constructor: either Bip with two parameters of types respectively a and b, or Bop with only one parameter of type a, or Bup with no parameters.
Make Blargh an instance of class Blup, where fisto is used to access to data of type a, and fosto to data of type b.

2.3 Instance II (4 points)

Define the sum type Blarf with two parameters of types a and b. It has two data constructor: either La and a list of elements of type a, or Lb and a list of elements of type b.
Make Blarf an instance of class Blup, where fisto is used to access to the head of the list of elements of type a, and fosto to the head of the list of elements of type b.

2.4
1. Smap (6 points)

Define a function smap that takes an infinite list L of Int, a function f from Int to Int, an operation OP over Int, and a threshold T. smap performs a map of f on L, while keeping an accumulator K (with starting value 0), which is updated at each step as oldAccumulatorV alue OP f(currentElementOfL). smap stops when the value of K reaches T and returns a list of all the computed values of the map. 
E.g. smap (^2) (+) [1,2..] 100 is the list [1,4,9,16,25,36,49].

2. Write smap’s type.

-}

class Blup a where
    fisto :: (a b c) -> Maybe b
    fosto :: (a b c) -> Maybe c


data Blargh a b = Bip a b | Bop a | Bup

instance Blup Blargh where
    fisto (Bip a b) = Just a
    fisto (Bop a) = Just a
    fisto Bup = Nothing

    fosto (Bip a b) = Just b
    fosto _ = Nothing

data Blarf a b = La [a] | Lb [b]

instance Blup Blarf where
    fisto (La []) = Nothing 
    fisto (La (x:xs)) = Just x
    fisto _ = Nothing

    fosto (Lb []) = Nothing
    fosto (Lb (x:xs)) = Just x
    fosto _ = Nothing   

smap :: (Int -> Int) -> (Int -> Int -> Int) -> [Int] -> Int -> [Int]
smap f1 f2 lst k = smap' f1 f2 lst k 0 [] where
    smap' f1 f2 (x:xs) k acc res =  let v1 = f1 x
                                        v2 = f2 acc v1
                                    in if acc < k then smap' f1 f2 xs k v2 (res ++ [v1])
                                                 else re
