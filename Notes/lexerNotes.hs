import Data.List
import Data.Char

{- unnecessary; does the same thing as span -}
splitWith :: (a -> Bool) -> [a] -> ([a],[a])
splitWith p [] = ([],[])
splitWith p (x:xs) | p x  = (x:r1,r2) where (r1,r2) = splitWith p xs
splitWith p (x:xs) | True = ([],x:xs)

type Vars = String
data AExpr = Var Vars | Const Integer | Add AExpr AExpr | Mul AExpr AExpr
           | Sub AExpr AExpr | Div AExpr AExpr
  deriving (Show,Eq)
data BinOps = PlusOp | MulOp | DivOp | SubOp
  deriving (Show,Eq)
data Token = BinOp BinOps | CSym Integer | VSym String | LPar | RPar | PE AExpr
  deriving (Show,Eq)

parseExpr :: [Token] -> AExpr
parseExpr ts = sr [] ts

sr :: [Token] -> [Token] -> AExpr
sr [PE e] [] = e
sr (VSym x : s) q = sr (PE (Var x) : s) q   --R1
sr (CSym c : s) q = sr (PE (Const c) : s) q --R2
sr (PE e2 : BinOp PlusOp : PE e1 : s) q = sr (PE (Add e1 e2) : s) q --R3
sr (PE e2 : BinOp MulOp : PE e1 : s) q = sr (PE (Mul e1 e2) : s) q --R3
sr (PE e2 : BinOp SubOp : PE e1 : s) q = sr (PE (Sub e1 e2) : s) q --R3
sr (PE e2 : BinOp DivOp : PE e1 : s) q = sr (PE (Div e1 e2) : s) q --R3
sr (PE e2 : ) 
sr (RPar : PE e : LPar : s) q          = sr (PE e : s) q
sr s      (x:xs) = sr (x:s) xs
sr s      [] = error ("Parse error: " ++ show s)

tokenEx :: [Token]
tokenEx = [ CSym 30, BinOp MulOp, LPar, VSym "x1", BinOp PlusOp, CSym 2
          , BinOp MulOp, VSym "y", RPar ]

-- lexer "30*(x1+2*y)" = tokenEx
lexer :: String -> [Token]
lexer "" = []
lexer ('(':s) = LPar : lexer s
lexer (')':s) = RPar : lexer s
lexer ('*':s) = BinOp MulOp : lexer s
lexer ('+':s) = BinOp PlusOp : lexer s
lexer ('/':s) = BinOp DivOp : lexer s
lexer ('-':s) = BinOp SubOp : lexer s
lexer (c:s) | isDigit c =
  let (num,rst) = span isDigit s
   in CSym (read (c:num)) : lexer rst
lexer (c:s) | isAlpha c =
  let (num,rst) = span (\x -> isAlphaNum x || x == '_') s
   in VSym (c:num) : lexer rst
lexer (c:s) | isSpace c = lexer s
lexer s = error ("Lexical error: " ++ s)

-- 2x+3y
-- aexpr :: AExpr
-- aexpr = Add (Mul (Const 2) (Var "x"))
--             (Mul (Const 3) (Var "y"))
--
-- parseAExpr :: String -> AExpr
-- parseAExpr = parser . lexer
-- -- parseAExpr s = parser (lexer s)
--
-- parser :: [Token] -> AExpr
-- parser = _
