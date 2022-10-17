
--Base Types: 
--Bool, Int/Integer. Float/Double, Char

--Type Constructs: 
--strings [a], functions a->b, pairs (a.b), Maybe a = Just a | Nothing

data Nat = Zero | Succ Nat

data List a = Nil | Cons a (List a)

data Tree a = Leaf | Node a (Tree a) (Tree a)

myTree :: Tree Integer
myTree = Node 5(Node 6 Leaf Leaf)
                (Node 7 (Node (-5) Leaf Leaf)
                        (Node 11 Leaf Leaf))

-- would look like:

--            5
--       6         7
--     -  -    -5    11
--             - -   - -

--map trees


mapTree :: (a->b) -> Tree a -> Tree b
mapTree f Leaf = Leaf
mapTree f (Node x t1 t2) = Node (f x) (mapTree f t1) (mapTree f t2)

--find a number that ends with 0 (divisable by 10)
findTen :: Tree Integer -> Maybe Integer
findTen Leaf = Nothing
findTen (Node x t1 t2) | mod x 10 == 0 = Just x
findTen (Node x t1 t2) = case findTen t1 of             
                                Nothing -> findTen t2
                                Just y -> Just y

--recurse on t1 first
    --If nothing, recurse on t2
    --If Integer, return int


findTen' :: Tree Integer -> Maybe Integer
findTen' Leaf = Nothing
findTen' (Node x t1 t2) | mod x 10 == 0 = Just x
findTen' (Node x t1 t2) | otherwise = maybe (findTen' t2) Just (findTen' t1)