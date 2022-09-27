
data Tree a = Leaf | Node a (Tree a) (Tree a)

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