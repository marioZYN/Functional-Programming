-- @Author: marioZhou
-- @Date:   2018-01-03 17:57:35
-- @Last Modified by:   marioZhou
-- @Last Modified time: 2018-01-03 18:55:17

data Clist a = Node a (Clist a) | End (Clist a)

instance Eq a => Eq (Clist a) where
    End _ == End _ = True
    (Node v next) == (Node v' next') = v == v' && next == next'
    _ == _ = False

list2clist :: [a] -> Clist a
list2clist [] = let node = End node
                in node
list2clist (x:xs) = let first = Node x $ list2clist' xs first
                    in first
list2clist' [] first = End first
list2clist' (x:xs) first = Node x $ list2clist' xs first

clist2list :: Clist a -> [a]
clist2list (End next) = clist2list next
clist2list (Node x next) = x : clist2list next

cmap :: (t -> a) -> Clist t -> Clist a
cmap f (Node x next) = let first = Node (f x) $ cmap' f next first
                       in first
cmap' f (End next) first =  (End first)
cmap' f (Node x next) first = Node (f x) $cmap' f next first