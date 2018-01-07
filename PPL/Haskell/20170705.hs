data Tree a = Nil | Leaf a | Branch (Tree a) (Tree a) deriving (Show,Eq)

tcompose :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
tcompose _ Nil _ = Nil
tcompose _ t Nil = t
tcompose f (Leaf a) (Leaf b) = Leaf $ f a b
tcompose f x@(Leaf a) (Branch l r) = Branch (tcompose f x l) (tcompose f x r)
tcompose f (Branch l r) t = Branch (tcompose f l t) (tcompose f r t)

t1 = Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3)
t2 = Branch (Leaf 6) (Leaf 7)

instance Foldable Tree where
    foldl _ acc Nil = acc
    foldl f acc (Leaf a) = f acc a
    foldl f acc (Branch l r) = foldl f (foldl f acc l) r
    foldr _ acc Nil = acc
    foldr f acc (Leaf a) = f a acc
    foldr f acc (Branch l r) = foldr f (foldr f acc r) l

revtree :: Tree a -> Tree a
revtree t = t1 where (t1, _) = revtree' t $ (reverse (foldr (\x y -> x:y) [] t))
                     revtree' Nil xs = (Nil, xs)
                     revtree' (Leaf v) (x:xs) = (Leaf x, xs)
                     revtree' (Branch l r) xs = let (l', xs') = revtree' l xs
                                                    (r', xs'') = revtree' r xs'
                                 
                                                in (Branch l' r', xs'')

instance Functor Tree where
    fmap f Nil = Nil
    fmap f (Leaf a) = Leaf $ f a
    fmap f (Branch l r) = Branch (fmap f l) (fmap f r) 

tcompose' :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
tcompose' _ Nil _ = Nil
tcompose' f (Leaf x) t2 = fmap (f x) t2
tcompose' f (Branch l r) t2 = Branch (tcompose' f l t2) (tcompose' f r t2)
