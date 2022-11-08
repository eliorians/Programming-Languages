import System.Win32 (COORD(x))

-- 1 -- done

data Safe a = Value a | Error String

unitSafe :: a -> Safe a
unitSafe x = Value x

bindSafe :: (a -> Safe b) -> Safe a -> Safe b
bindSafe f (Value x) = f x
bindSafe f (Error x) = Error x

instance Functor Safe where
    fmap f = bindSafe (unitSafe . f)

instance Applicative Safe where
    pure    = unitSafe
    f <*> t = bindSafe (<$> t) f

instance Monad Safe where
    return  = unitSafe
    t >>= f = bindSafe f t


-- 2 --

data FTree a = Leaf a | Node [FTree a]

unitFTree :: a -> FTree a
unitFTree x = Leaf x 

bindFTree :: (a -> FTree b) -> FTree a -> FTree b
bindFTree f (Leaf x) = f x
bindFTree f (Node x:xs) = map (bindFTree f) xs

instance Functor FTree where
    fmap f = bindFTree (unitFTree . f)

instance Applicative FTree where
    pure    = unitFTree
    f <*> t = bindFTree (<$> t) f

instance Monad FTree where
    return  = unitFTree
    t >>= f = bindFTree f t


-- 3 --

data Prop a = PVar a | TT | FF | And (Prop a) (Prop a) | Or (Prop a) (Prop a)
    | Not (Prop a) | Imp (Prop a) (Prop a) | Iff (Prop a) (Prop a)

unitProp :: a -> Prop a
unitProp x = PVar x 

bindProp :: (a -> Prop b) -> Prop a -> Prop b
bindProp f TT = TT
bindProp f FF = FF
bindProp f (PVar x) = f x
bindProp f (Not x) =  Not (bindProp f x)
bindProp f (And x y) = And (bindProp f x) (bindProp f y)
bindProp f (Or x y) = Or (bindProp f x) (bindProp f y)
bindProp f (Iff x y) = Iff (bindProp f x) (bindProp f y)
bindProp f (Imp x y) = Imp (bindProp f x) (bindProp f y)

instance Functor Prop where
    fmap f = bindProp (unitProp . f)

instance Applicative Prop where
    pure    = unitProp
    f <*> t = bindProp (<$> t) f

instance Monad Prop where
    return  = unitProp
    t >>= f = bindProp f t


-- 4 --

data Lam a = Var a | App (Lam a) (Lam a) | Abs (Lam (Maybe a))


unitLam :: a -> Lam a
unitLam x = Var x

bindLam :: (a -> Lam b) -> Lam a -> Lam b
bindLam f (Var x) = f x
bindLam f (App x y) = App (bindLam f x) (bindLam f y)
bindLam f (Abs x) = Abs (bindLam (lift f) x)

lift :: (a -> Lam b) -> Maybe a -> Lam (Maybe b)
lift f (Just a)  =  fmap (a) (f a)
lift f Nothing = Nothing

instance Functor Lam where
    fmap f = bindLam (unitLam . f)

instance Applicative Lam where
    pure    = unitLam
    f <*> t = bindLam (<$> t) f

instance Monad Lam where
    return  = unitLam
    t >>= f = bindLam f t


lam :: Lam Bool
lam = App (Var True) (Abs (App (Var (Just False)) (Var Nothing)))

-- 5 --

data Poly a = Mono Double [a] | Sum (Poly a) (Poly a)

--unitPoly :: a -> Poly a
--unitPoly x = Mono x

--bindPoly :: (a -> Poly b) -> Poly a -> Poly b