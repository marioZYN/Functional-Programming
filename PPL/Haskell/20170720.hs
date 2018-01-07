data Ttree a :: Tleaf Color a| Tnode Color a  (Ttree a) (Ttree a) (Ttree a) deriving (Show, Eq)

data Color :: Yellow | Blue deriving (Show, Eq)

instance Functor Ttree where
	fmap f (Tleaf c v) = Tleaf c $ f v
	fmap f (Node  c v l m r) = Tnode c (f v) (fmap f l) (fmap f m) (fmap f r)

instance Foldable Ttree where

	foldr f x Tleaf _ a = f a x 
	foldr f x (Tnode _ a l m r) = 
		let v1 = foldr f x r
			v2 = foldr f v1 m
			v3 = foldr f v2 l
		in f a v3

noblues :: Ttree a -> Bool
noblues (Tleaf Blue _) = False
noblues (Tleaf Yellow _) = True
noblues (Tnode Blue _ _ _ ) = False
noblues (Tnode Yellow _ l m r) = (noblues l) && (noblues m) && (noblues r)

yellowSubTrees :: (Ttree a) -> [Ttree a]
yellowSubTrees t = yellowSubTrees' t [] where
	yellowSubTrees' x@(Tleaf Blue _) xs = xs
	yellowSubTrees' x@(Tleaf Yellow _) xs = x:xs
	yellowSubTrees' x@(Tnode _ _ l m r) xs = 
		| (noblues x) = x:xs
		| otherwise = (yellowSubTrees' l xs) ++ (yellowSubTrees' m xs) ++ (yellowSubTrees' r xs)