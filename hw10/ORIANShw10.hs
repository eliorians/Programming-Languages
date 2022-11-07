

-- 1 -- done

data Safe a = Value a | Error String

unitSafe :: a -> Safe a
unitSafe x = Value x

bindSafe :: (a -> Safe b) -> Safe a -> Safe b
bindSafe f (Value x) = f x
bindSafe f (Error x) = error x

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
bindFTree f (Node x) = bindFTree f x



-- 3 --

data Prop a = PVar a | TT | FF | And (Prop a) (Prop a) | Or (Prop a) (Prop a)
    | Not (Prop a) | Imp (Prop a) (Prop a) | Iff (Prop a) (Prop a)

unitProp :: a -> Prop a
unitProp x = PVar x 

bindProp :: (a -> Prop b) -> Prop a -> Prop b
bindProp f TT = True
bindProp f FF = False
bindProp f (PVar x) = f x
bindProp f (Not x) = not (f x)
bindProp f (And x y) = (bindProp f x) && (bindProp f y)
bindProp f (Or x y) = (bindProp f x) && (bindProp f y)
bindProp f (Imp x y) = (bindProp f x) && (bindProp f y)
bindProp f (Iff x y) = (bindProp f x) && (bindProp f y)




-- 4 --

data Lam a = Var a | App (Lam a) (Lam a) | Abs (Lam (Maybe a))

lam :: Lam Bool
lam = App (Var True) (Abs (App (Var (Just False)) (Var Nothing)))

-- 5 --

data Poly a = Mono Double [a] | Sum (Poly a) (Poly a)