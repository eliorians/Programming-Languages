import Data.Char
import Data.List

type Vars = String
data Prop = Var Vars | Const Bool | And Prop Prop | Or Prop Prop | Not Prop
            | Imp Prop Prop | Iff Prop Prop | Xor Prop Prop
    deriving (Show, Eq)
    --implication , equivalance, eXclusive OR

prop1 = Var "X" `And` Var "Y"               -- X /\ Y
prop2 = Var "X" `Imp` Var "Y"               -- X -> Y
prop3 = Not (Var "X") `Or` Var "Y"          -- !X \/ Y
prop4 = Not (Var "X") `Iff` Not (Var "Y")   -- !X <-> !Y

-- binary operators
data BOps = AndOp | OrOp | ImpOp | IffOp | XorOp
    deriving (Show,Eq)
-- the type of tokens
data Token = VSym Vars | CSym Bool | BOp BOps | NotOp | LPar | RPar
            | PB Prop -- a token to store parsed boolean expressions
    deriving (Show,Eq)


---------------------------


--2.1 Evaluator done

type Env = [(Vars,Bool)]
eval :: Env -> Prop -> Bool
eval env (Var x) = case lookup x env of
    Nothing -> error $ "No value for variable " ++ x
    Just v  -> v
eval env (Const b) = b
eval env (And f1 f2) = eval env f1 && eval env f2
eval env (Or f1 f2)  = eval env f1 || eval env f2
eval env (Not f) = not (eval env f)
eval env (Imp f1 f2) = eval env f1 <= eval env f2     --f1 implies f2
eval env (Iff f1 f2) = eval env f1 == eval env f2
eval env (Xor f1 f2) = eval env f1 /= eval env f2



--2.2 Lexical Analysis done

lexer :: String -> [Token]
--Base
lexer "" = []
lexer (c:s) | isSpace c = lexer s
--VSym
lexer (c:s) | isAlpha c =
  let (num,rst) = span (\x -> isAlphaNum x || x == '_') s
   in VSym (c:num) : lexer rst
--CSym
lexer (c:s) | isDigit c =
  let (num,rst) = span isDigit s
   in CSym (read (c:num)) : lexer rst
--BOps
lexer ('/':'\\':s) = BOp AndOp : lexer s
lexer ('\\':'/':s) = BOp OrOp : lexer s
lexer ('-' : '>' :s) = BOp ImpOp : lexer s
lexer ('<' : '-' : '>' :s) = BOp IffOp : lexer s
lexer ('<' : '+' : '>':s) = BOp XorOp : lexer s
--NotOp
lexer ('!':s) = NotOp : lexer s
--Pars
lexer ('(':s) = LPar : lexer s
lexer (')':s) = RPar : lexer s
--PB Prop
lexer s = error("Unrecognized Token: " ++ s)


--2.3 Parser done
parseProp :: [Token] -> Prop
parseProp ts = sr [] ts

sr :: [Token] -> [Token] -> Prop
--Base
sr [PB e] [] = e
--VSym & CSym
sr (VSym x : s) q = sr (PB (Var x) : s) q   --R1
sr (CSym c : s) q = sr (PB (Const c) : s) q --R2
--Bops
sr (PB e2 : BOp AndOp : PB e1 : s) q = sr (PB (And e1 e2) : s) q --R3
sr (PB e2 : BOp OrOp : PB e1 : s) q = sr (PB (Or e1 e2) : s) q --R3
sr (PB e2 : BOp ImpOp : PB e1 : s) q = sr (PB (Imp e1 e2) : s) q --R3
sr (PB e2 : BOp IffOp : PB e1 : s) q = sr (PB (Iff e1 e2) : s) q --R3
sr (PB e2 : BOp XorOp : PB e1 : s) q = sr (PB (Xor e1 e2) : s) q --R3
sr (PB e : NotOp : s) q = sr (PB (Not e) : s) q
--LPar, R Par
sr (RPar : PB e : LPar : s) q = sr (PB e : s) q
sr s (x:xs) = sr (x:s) xs
sr s [] = error ("Parse error: " ++ show s)


--2.4 Searching for Truth Statment


fv :: Prop -> [Vars]
fv (Var v) = [v]
fv (Const b) = []
fv (And x y) = noDups (fv x ++ fv y)
fv (Or x y) = noDups (fv x ++ fv y)
fv (Not x) = noDups (fv x)
fv (Imp x y) = noDups (fv x ++ fv y)
fv (Iff x y) = noDups (fv x ++ fv y)
fv (Xor x y) = noDups (fv x ++ fv y)

noDups :: Eq a => [a] -> [a]
noDups [] = []
noDups (x:xs) = x : noDups (filter (/= x) xs)

genEnvs :: [Vars] -> [Env]
genEnvs [] = [[]]
genEnvs (x:xs) = [ y:ys | y <- extend x, ys <- genEnvs xs ]

extend :: Vars -> [(Vars, Bool)]
extend x = [ (x,b) | b <- [True, False] ]

evalAll :: Prop -> [Env] -> Env
evalAll p [] = []
evalAll p (key:keys) = if eval key p then key else evalAll p keys

findSat :: Prop -> Maybe Env
findSat x = evalAll x (genEnvs(fv x))


--2.5 Putting it together
solve :: String -> String
solve s = if(findsat(parseProp(lexer s))) then "Satisfiable" else "No Solution"