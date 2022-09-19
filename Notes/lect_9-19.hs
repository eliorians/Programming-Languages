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


