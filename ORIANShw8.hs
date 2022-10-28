module Prop where
import Data.Char
import Data.List



-- 2 Read-eval-print loop



--3 File I/O







-- ! Homework 7 Setup ! --

-- Propositional Logic setup
type Vars = String
data Prop = Var Vars | Const Bool | And Prop Prop | Or Prop Prop | Not Prop
          | Imp Prop Prop | Iff Prop Prop | Xor Prop Prop
  deriving (Show,Eq)

type Env = [(Vars,Bool)]

lookUp :: Vars -> [(Vars,Bool)] -> Bool
lookUp x [] = error ("Cannot find variable " ++ x ++ " in the environment.")
lookUp x ((key,val):r) = if key == x then val else lookUp x r

-- 2.1 Evaluator

eval :: [(Vars,Bool)] -> Prop -> Bool
eval env (Var x)     = case (lookup x env) of
                         Nothing -> error $ "No value for variable " ++ x
                         Just v  -> v
eval env (Const b)   = b
eval env (And f1 f2) = eval env f1 && eval env f2
eval env (Or f1 f2)  = eval env f1 || eval env f2
eval env (Not f)     = not (eval env f)
eval env (Imp x y)   = if eval env x then eval env y else True
eval env (Iff x y)   = eval env x == eval env y
eval env (Xor x y)   = eval env x /= eval env y

-- 2.2 Lexical Analysis

-- binary operators
data BOps = AndOp | OrOp | ImpOp | IffOp | XorOp
  deriving (Show,Eq)
-- the type of tokens
data Token = VSym Vars | CSym Bool | BinOp BOps | NotOp | LPar | RPar
           | PB Prop  -- a token to store parsed boolean expressions
  deriving (Show,Eq)

lexer :: String -> [Token]
lexer ""                = []
lexer ('('         : s) = LPar        : lexer s
lexer (')'         : s) = RPar        : lexer s
lexer ('/':'\\'    : s) = BinOp AndOp : lexer s
lexer ('\\':'/'    : s) = BinOp OrOp  : lexer s
lexer ('-':'>'     : s) = BinOp ImpOp : lexer s
lexer ('<':'-':'>' : s) = BinOp IffOp : lexer s
lexer ('<':'+':'>' : s) = BinOp XorOp : lexer s
lexer ('!'         : s) = NotOp       : lexer s
lexer ('t':'t'     : s) = CSym True   : lexer s
lexer ('f':'f'     : s) = CSym False  : lexer s
lexer (c:s) | isUpper c = let (var,rst) = span isAlphaNum s
                           in VSym (c:var) : lexer rst
lexer (c:s) | isSpace c = lexer s
lexer s = error ("Lexical error: " ++ s)

-- 2.3 Parser

parseProp :: [Token] -> Prop
parseProp ts = sr [] ts

sr :: [Token] -> [Token] -> Prop
sr [PB e]                           [] = e
sr (VSym x : s)                      q = sr (PB (Var x)     : s) q
sr (CSym y : s)                      q = sr (PB (Const y)   : s) q
sr (PB e2 : BinOp AndOp : PB e1 : s) q = sr (PB (And e1 e2) : s) q
sr (PB e2 : BinOp OrOp : PB e1 : s)  q = sr (PB (Or  e1 e2) : s) q
sr (PB e2 : BinOp ImpOp : PB e1 : s) q = sr (PB (Imp e1 e2) : s) q
sr (PB e2 : BinOp IffOp : PB e1 : s) q = sr (PB (Iff e1 e2) : s) q
sr (PB e2 : BinOp XorOp : PB e1 : s) q = sr (PB (Xor e1 e2) : s) q
sr (PB e : NotOp : s)                q = sr (PB (Not e)     : s) q
sr (RPar : PB e : LPar : s)          q = sr (PB e           : s) q
sr s                             (x:q) = sr (x:s)                q
sr s                                [] = error ("Parse error: " ++ show s)

-- 2.4 Finding a satisfying assignment

removeDups :: (Eq a) => [a] -> [a]
removeDups = foldr (\x -> (x :) . filter (/= x)) []

fv :: Prop -> [Vars]
fv (Var x) = [x]
fv (And f1 f2) = removeDups (fv f1 ++ fv f2)
fv (Or  f1 f2) = removeDups (fv f1 ++ fv f2)
fv (Imp f1 f2) = removeDups (fv f1 ++ fv f2)
fv (Iff f1 f2) = removeDups (fv f1 ++ fv f2)
fv (Xor f1 f2) = removeDups (fv f1 ++ fv f2)
fv (Not f)     = fv f
fv _ = []

genEnvs :: [Vars] -> [Env]
genEnvs = foldr (\x y -> map ((x,True):) y ++ map ((x,False):) y) [[]]

findSat :: Prop -> Maybe Env
findSat p = find (flip eval p) (genEnvs (fv p))

-- 2.5 Putting it all together

solve :: String -> String
solve s = case (findSat (parseProp (lexer s))) of
  Nothing -> "No solution."
  Just _  -> "Satisfiable."


-- Testing examples
prop1 = Var "X" `And` Var "Y"
prop2 = Var "X" `Imp` Var "Y"
prop3 = Not (Var "X") `Or` (Var "Y")
prop4 = Not (Var "X") `Iff` Not (Var "Y")