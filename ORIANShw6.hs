data LTree a = LLeaf a | LNode a (LTree a) (LTree a)
    deriving Show

--comb is the recursive function passed (eg getLeaves t1)
--base is the base case (eg leaf x)
--and the current node/leaf
foldTree :: (a -> b -> b -> b) -> (a -> b) -> LTree a -> b
foldTree comb base (LLeaf x) = base x
foldTree comb base (LNode y t1 t2) = comb y (foldTree comb base t1) (foldTree comb base t2)

--1 recursion done
getLeaves :: LTree a -> [a]
getLeaves (LLeaf x) = [x]
getLeaves (LNode x t1 t2) = getLeaves t1 ++ getLeaves t2

--1 fold done
getLeaves' :: LTree a -> [a]
getLeaves' = foldTree (\x l r -> l ++ r) (\x -> [x])

--2 recursion done
countNodes :: LTree a -> Integer
countNodes (LLeaf x) = 0
countNodes (LNode x t1 t2) = 1 + countNodes t1 + countNodes t2

--2 fold done
countNodes' :: LTree a -> Integer
countNodes' = foldTree (\x l r -> 1 +  l +  r) (\x -> 0)

--3 recursion done
sumTree :: LTree Integer -> Integer
sumTree (LLeaf x) = x
sumTree (LNode x t1 t2) = x + sumTree t1 + sumTree t2

--3 fold done
sumTree' :: LTree Integer -> Integer
sumTree' = foldTree (\x l r -> x + l + r) (\x -> x)

--4 recursion done
occursInLeaves :: (a -> Bool) -> LTree a -> Bool
occursInLeaves f (LLeaf x) = f x
occursInLeaves f (LNode x t1 t2) = occursInLeaves f t1 || occursInLeaves f t2

--4 fold done
occursInLeaves' :: (a -> Bool) -> LTree a -> Bool
occursInLeaves' f = foldTree (\x l r -> l || r) (\x -> f x)

--5 recursion done
checkNoCover :: (Eq a) => a -> LTree a -> Bool
checkNoCover f (LLeaf x) = f == x
checkNoCover f (LNode x t1 t2) = if f == x then False else checkNoCover f t1 || checkNoCover f t2

--5 fold done
checkNoCover' :: (Eq a) => a -> LTree a -> Bool
checkNoCover' f t = foldTree (\x l r -> if f == x then False else l || r) (\x -> f == x) t

--first lambda is Node, with \x being the data and l and r being your left and right
--second lambda is your Leaf with \x being the content
--final input is the tree