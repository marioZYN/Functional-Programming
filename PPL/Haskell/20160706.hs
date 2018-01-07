-- @Author: YINAN
-- @Date:   2017-12-29 08:58:06
-- @Last Modified by:   marioZhou
-- @Last Modified time: 2018-01-03 15:52:22

 
--1. Define a generic tree data structure, called Gtree, for trees having any number of children.
--2. Make Gtree an instance of Functor.
--3. Make Gtree an instance of Applicative, with <*> working like ftree in Exercise 1, but for the empty
--first parameter (i.e. the two arguments of <*> must necessarily have the same structure).
--4. Is it possible to define a <*> operation which works exactly like ftree (of course, with the hypothesis
--of having homogeneous Gtrees)? If the answer is yes, implement it; if no, explain why.

data Gtree a = Leaf a | Node [Gtree a] deriving (Show, Eq)

instance Functor Gtree where
    fmap f (Node []) = Node []
    fmap f (Leaf v) = Leaf (f v)
    fmap f (Node (x:xs)) = Node ((fmap f x) : vs) where
        Node vs = fmap f (Node xs)

ftree :: Gtree (a -> b) -> Gtree a -> Gtree b
ftree (Node []) (Node []) = Node []
ftree (Leaf f) (Leaf v) = Leaf (f v)
ftree (Node (f:fs)) (Node (x:xs)) = Node ((ftree f x) : vs) where
    Node vs = ftree (Node fs) (Node xs)

instance Applicative Gtree where
    pure = Leaf
    (<*>) = ftree