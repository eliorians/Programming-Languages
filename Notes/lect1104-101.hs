data List a = Nil | Cons a (List a)
  deriving Show

encode :: [a] -> List a
encode [] = Nil
encode (x:xs) = Cons x (encode xs)

decode :: List a -> [a]
decode Nil = []
decode (Cons x xs) = x : decode xs

instance Eq a => Eq (List a) where
  Nil == Nil = True
  (Cons x xs) == (Cons y ys) = (x == y) && (xs == ys)
  _ == _ = False

instance Functor List where
  fmap f Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

appendList :: List a -> List a -> List a
appendList Nil xs = xs
appendList (Cons y ys) xs = Cons y (appendList ys xs)

bindList :: (a -> List b) -> List a -> List b
bindList f Nil = Nil
bindList f (Cons x xs) = appendList (f x) (bindList f xs)

instance Applicative List where
  pure x = Cons x Nil -- [x]
  fs <*> fx = bindList (\f -> f <$> fx) fs

instance Monad List where
  return x = Cons x Nil
  xs >>= f = bindList f xs

data Maybe' a = Nothing' | Just' a deriving Show

instance Functor Maybe' where
  fmap f (Nothing') = Nothing'
  fmap f (Just' x)  = Just' (f x)

unitMaybe :: a -> Maybe' a
unitMaybe x = Just' x

bindMaybe :: (a -> Maybe' b) -> Maybe' a -> Maybe' b
bindMaybe f Nothing' = Nothing'
bindMaybe f (Just' x) = f x

instance Applicative Maybe' where
  pure = unitMaybe
  fs <*> fx = bindMaybe (\f -> f <$> fx) fs

instance Monad Maybe' where
  return = unitMaybe
  xs >>= f = bindMaybe f xs

data Tree a = Leaf | Node a (Tree a) (Tree a)

instance Functor Tree where
  fmap f Leaf = Leaf
  fmap f (Node x t1 t2) = Node (f x) (fmap f t1) (fmap f t2)

data Expr a = Var a | Const Integer
            | Add (Expr a) (Expr a) | Mul (Expr a) (Expr a)
  deriving Show

instance Functor Expr where
  fmap f (Var x) = Var (f x)
  fmap f (Const n) = Const n
  fmap f (Add e1 e2) = Add (fmap f e1) (fmap f e2)
  fmap f (Mul e1 e2) = Mul (fmap f e1) (fmap f e2)

unitExpr :: a -> Expr a
unitExpr x = Var x

bindExpr :: (a -> Expr b) -> Expr a -> Expr b
bindExpr f (Var x) = f x
bindExpr f (Const n) = Const n
bindExpr f (Add e1 e2) = Add (bindExpr f e1) (bindExpr f e2)
bindExpr f (Mul e1 e2) = Mul (bindExpr f e1) (bindExpr f e2)

instance Applicative Expr where
  pure = unitExpr
  ef <*> ex = bindExpr (<$> ex) ef

instance Monad Expr where
  return = unitExpr
  ex >>= f = bindExpr f ex
