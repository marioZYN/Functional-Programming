data LolStream x = LolStream Int [x]

lolstream lol = let v = lol ++ v
                in concat v

lol2lolstream lol = let v = lolstream lol
                        n = sum $ map length lol
                    in LolStream n v

-- utilities
destream (LolStream v l) = if v < 0 then l else take v l
periodic (LolStream v _) = v >= 0

-- instances
instance Eq a => Eq (LolStream a) where
  v == v1 = (destream v) == (destream v1)

instance Functor LolStream where
  fmap f (LolStream n l) = LolStream n (map f l)

instance Applicative LolStream where
  pure v = lol2lolstream [[v]]
  v1@(LolStream n1 f) <*> v2@(LolStream n2 g) =
    LolStream (n1 * n2) $ lol2stream [(destream v1) <*> (destream v2)]

instance Foldable LolStream where
  foldr f v t = foldr f v $ destream t

instance Monad LolStream where
  v >>= f = lol2lolstream [destream v >>= \x -> destream (f x)]