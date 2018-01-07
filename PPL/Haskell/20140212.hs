-- @Author: YINAN
-- @Date:   2017-12-29 08:58:06
-- @Last Modified by:   marioZhou
-- @Last Modified time: 2018-01-06 20:02:16


{-
2 Haskell 

2.1 Tree (2 pts)
Define a Tree data structure, where each node contains a value and can have any number of children. 

2.2 Visit (4 pts)
Define a visit function, that returns a list of all the elements that are contained in the tree data structure defined before (you can choose any order you like).

2.3 Equality (2 pts)
Two trees are considered equal iff they contain the same elements and those are in the order defined by the vist function defined before (so they could be structurally different). Define == for Tree.

2.4 zipToList (4 pts)
Define the zipToList :: [(a,a)] -> [a] function, that, given a list of pairs, returns a flat list containing all the elements found in the pairs. E.g. zipToList [(1,2),(3,4)] is [1,2,3,4].

2.5 Free monoid (4 pts)
Define an infinite list containing all the elements of the free monoid {a, b}∗ (i.e. all the strings defined on the alphabet {a, b}, empty string included).

-}

data Tree a = Node a [Tree a] deriving Show

x = Node 0 [(Node 1 []), (Node 2 [])]
y = Node 3 [(Node 4 []), (Node 5 [x])]

visit :: Tree a -> [a]
visit (Node v []) = [v]
visit (Node v (x:xs)) = (visit x) ++ (visit (Node v xs))

instance Eq a => Eq (Tree a) where
    t1 == t2 = (visit t1) == (visit t2)

zipToList :: [(a, a)] -> [a]
zipToList lst = zipToList' lst [] where
    zipToList' [] res = res
    zipToList' ((x1, x2):xs) res = zipToList' xs (res ++ [x1, x2])

