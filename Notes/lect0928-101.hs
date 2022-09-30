
data Tree a = Leaf | Node a (Tree a) (Tree a)
  deriving Show

myTree :: Tree Integer
myTree = Node 3 (Node 4 (Node (-1) Leaf Leaf)
                        (Node 0 Leaf Leaf))
                (Node 5 Leaf Leaf)

myTree2 :: Tree Float
myTree2 = Node 3.0 (Node 4.2 (Node (-1.5) Leaf Leaf)
                        (Node 0 Leaf Leaf))
                (Node pi Leaf Leaf)

negateTree :: Tree Integer -> Tree Integer
negateTree Leaf = Leaf
negateTree (Node x t1 t2) = Node (-x) (negateTree t1) (negateTree t2)

stringify :: Tree Float -> Tree String
stringify Leaf = Leaf
stringify (Node x t1 t2) = Node (show x) (stringify t1) (stringify t2)

-- find a number that ends with a 0, i.e., divisible by 10
findRound :: Tree Integer -> Maybe Integer
findRound Leaf = Nothing
findRound (Node x t1 t2) | mod x 10 == 0 = Just x
findRound (Node x t1 t2) | otherwise     = maybe (findRound t2) Just (findRound t1)
-- findRound (Node x t1 t2) | otherwise     = case findRound t1 of
--                                              Nothing -> findRound t2
--                                              Just y  -> Just y

-- data Tree a = Leaf | Node a (Tree a) (Tree a)
--   deriving Show

findCapital :: Tree String -> Maybe String
findCapital Leaf = Nothing
findCapital (Node (x:xs) t1 t2) | elem x ['A'..'Z'] = Just (x:xs)
findCapital (Node _ t1 t2) = maybe (findCapital t2) Just (findCapital t1)

findRound' :: Tree Integer -> Maybe Integer
findRound' Leaf = Nothing
findRound' (Node x t1 t2) = if mod x 10 == 0
                              then Just x
                              else maybe (findRound t2) Just (findRound t1)

findCapital' :: Tree String -> Maybe String
findCapital' Leaf = Nothing
findCapital' (Node x t1 t2) = if not (null x) && head x `elem` ['A'..'Z']
                                 then Just x
                                 else maybe (findCapital t2) Just (findCapital t1)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Leaf = Leaf
mapTree f (Node x t1 t2) = Node (f x) (mapTree f t1) (mapTree f t2)

mapTree' :: (a -> b) -> Tree a -> Tree b
mapTree' f t = foldTree (\x y1 y2 -> Node (f x) y1 y2) Leaf t

findTree :: (a -> Bool) -> Tree a -> Maybe a
findTree p Leaf = Nothing
findTree p (Node x t1 t2) = if p x
                              then Just x
                              else maybe (findTree p t2) Just (findTree p t1)

findTree' :: (a -> Bool) -> Tree a -> Maybe a
findTree' p t = foldTree (\x y1 y2 -> if p x then Just x else maybe y2 Just y1)
                         (Nothing)
                         t

foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree rf bc Leaf = bc
foldTree rf bc (Node x t1 t2) = rf x (foldTree rf bc t1) (foldTree rf bc t2)


myTree3 :: Tree String
myTree3 = Node "there" (Node "hey" Leaf Leaf)
                         (Node "World" Leaf Leaf)

concatTree :: Tree String -> String
concatTree Leaf = ""
concatTree (Node s t1 t2) = concatTree t1 ++ s ++ concatTree t2
