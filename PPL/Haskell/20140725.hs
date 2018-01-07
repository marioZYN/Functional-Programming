-- @Author: YINAN
-- @Date:   2017-12-29 08:58:06
-- @Last Modified by:   marioZhou
-- @Last Modified time: 2018-01-06 11:32:07


{-
Consider this data definition: data Valn a = Valn a (a -> Bool)
where a is a generic type, and the function: a -> Bool is a predicate that checks the validity of the
stored value.
1) Valn cannot derive Eq or Show, why?
Answer => Because data Valn contains a function (a -> Bool) and in Haskell we don't have standard representation for functions

-}
data Valn a = Valn a (a -> Bool)

instance Eq a => Eq (Valn a) where
    (Valn x1 f1) == (Valn x2 f2) = (x1 == x2) && (f1 x1) == (f2 x2)

instance Show a => Show (Valn a) where
    show (Valn x f) = (show x) ++ (show $ f x)

{-
Make Valn an instance of Num, considering that the predicate for two argument functions
(e.g. (+)) must be the logical “and” of the two predicates; for one argument functions, say abs, the predicate remains the same.

-}

instance Num a => (Valn a) where
    (Valn x1 f1) + (Valn x2 f2) = Valn (x1+x2) (\x -> (f1 x) && (f2 x))
    (Valn x1 f1) - (Valn x2 f2) = Valn (x1-x2) (\x -> (f1 x) && (f2 x))
    (Valn x1 f1) * (Valn x2 f2) = Valn (x1*x2) (\x -> (f1 x) && (f2 x))
    (Valn x1 f1) / (Valn x2 f2) = Valn (x1/x2) (\x -> (f1 x) && (f2 x))
    negate (Valn x f) = Valn (negate x) f
    abs (Valn x f) = Valn (abs x) f
    signum (Valn x f) = Valn (signum x) f
    fromInteger i = Valn (fromInteger i) (\x -> True)

