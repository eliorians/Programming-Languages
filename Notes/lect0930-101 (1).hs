
data Maybe' a = Nothing' | Just' a

--    [a] ::=  [] |  (:) a [a]
data List a = Nil | Cons a (List a)

foldList :: (a -> b -> b) -> b -> List a -> b
foldList rf bc Nil = bc
foldList rf bc (Cons x xs) = rf x (foldList rf bc xs)

data Tree a = Leaf | Node a (Tree a) (Tree a)

foldTree :: (a -> b -> b -> b) -> b -> Tree a -> b
foldTree rf bc Leaf = bc
foldTree rf bc (Node x t1 t2) = rf x (foldTree rf bc t1) (foldTree rf bc t2)

data LTree a = LLeaf a | LNode a (LTree a) (LTree a)

foldLTree :: (a -> b -> b -> b) -> (a -> b) -> LTree a -> b
foldLTree rf bc (LLeaf x) = bc x
foldLTree rf bc (LNode x t1 t2) = rf x (foldLTree rf bc t1) (foldLTree rf bc t2)

-- the type of arithmetic expressions
data AExpr = Var String | Const Integer | Add AExpr AExpr | Mul AExpr AExpr
  deriving Show

foldAExpr :: (String -> b) -> (Integer -> b) -> (b -> b -> b) -> (b -> b -> b) -> AExpr -> b
foldAExpr vc cc ac mc (Var v) = vc v
foldAExpr vc cc ac mc (Const n) = cc n
foldAExpr vc cc ac mc (Add a1 a2) = ac (foldAExpr vc cc ac mc a1) (foldAExpr vc cc ac mc a2)
foldAExpr vc cc ac mc (Mul a1 a2) = mc (foldAExpr vc cc ac mc a1) (foldAExpr vc cc ac mc a2)

-- 3x+2y
myAExpr :: AExpr
myAExpr = Add (Mul (Const 3) (Var "x"))
              (Mul (Const 2) (Var "y"))

getVars :: AExpr -> [String]
getVars (Var v) = [v]
getVars (Const n) = []
getVars (Add a1 a2) = getVars a1 ++ getVars a2
getVars (Mul a1 a2) = getVars a1 ++ getVars a2

apostrophize :: AExpr -> AExpr
apostrophize (Var v) = Var (v ++ "\'")
apostrophize (Const n) = Const n
apostrophize (Add a1 a2) = Add (apostrophize a1) (apostrophize a2)
apostrophize (Mul a1 a2) = Mul (apostrophize a1) (apostrophize a2)



-- the type of boolean expressions
data BExpr = BVar String | TT | FF | And BExpr BExpr | Or BExpr BExpr | Not BExpr

data Instr = Assign String AExpr  -- X := X + 1
           | IfThenElse BExpr Instr Instr
           | While BExpr [Instr]
           | Skip
           | Return AExpr
