
import Data.Char
import Data.List

--Variables
type Vars = String

--Arithmetic expressions
data AExpr = Var Vars | Const Integer -- variables and constants
    | Add AExpr AExpr | Sub AExpr AExpr -- addition and subtraction
    | Mul AExpr AExpr | Div AExpr AExpr -- multiplication and division
    | Exp AExpr AExpr | Mod AExpr AExpr -- exponential and remainder
    deriving Show

-- Boolean expressions
data BExpr = TT | FF | Not BExpr -- the true and false constants
    | And BExpr BExpr | Or BExpr BExpr -- and the boolean operations
    | Eq AExpr AExpr -- equality of arithmetic expressions
    | Lt AExpr AExpr -- true if the first is less than the second
    | Lte AExpr AExpr -- true if it's less than or equal to
    deriving Show

-- Instructions
data Instr = Assign Vars AExpr -- assign X to the value of an expression
    | IfThenElse BExpr Instr Instr -- conditional
    | While BExpr Instr -- looping construct
    | Do [Instr] -- a block of several instructions
    | Nop -- the "do nothing" instruction
    deriving Show

type Env = [(Vars,Integer)]





-- !Q1 eval arthimitic and boolean expressions

evala :: Env -> AExpr -> Integer
evala env (Const x) = x
evala env (Var x)   = case lookup x env of
                        Nothing -> error $ "No value for variable " ++ x
                        Just v  -> v
evala env (Add x y) = evala env x + evala env y
evala env (Sub x y) = evala env x - evala env y
evala env (Mul x y) = evala env x * evala env y
evala env (Div x y) = evala env x `div` evala env y
evala env (Exp x y) = evala env x ^ evala env y
evala env (Mod x y) = evala env x `mod` evala env y


evalb :: Env -> BExpr -> Bool
evalb env TT = True
evalb env FF = False
evalb env (Not x) = not (evalb env x)
evalb env (And x y) = evalb env x && evalb env y
evalb env (Or x y) = evalb env x || evalb env y
evalb env (Eq x y) = evala env x == evala env y
evalb env (Lt x y) = evala env x < evala env y
evalb env (Lte x y) = evala env x <= evala env y


-- !Q2 execute instructions

-- update (x,v) e sets the value of x to v and keeps other variables in e the same
update :: (Vars, Integer) -> Env -> Env
update (x,v) [] = [(x,v)]
update (x,v) ((y,w):es) | x == y = (x,v) : es
                        | otherwise = (y,w) : update (x,v) es

exec :: Instr -> Env -> Env
exec (Assign v a) env = update (v, evala env a) env
exec (IfThenElse b i1 i2) env = execIf b i1 i2 env
exec (While b i) env =  execWhile b i env
exec (Do is) env = execDo is env
exec (Nop) env = env

execList :: [Instr] -> Env -> Env
execList [] = id
execList (i:is) = execList is . exec i

execIf :: BExpr -> Instr -> Instr -> Env -> Env
execIf b i1 i2 env = if (evalb env b) then exec i1 env else exec i2 env

execWhile :: BExpr -> Instr -> Env -> Env
execWhile b i env = if (evalb env b) then exec (While b i) (exec i env) else exec Nop env

execDo :: [Instr] -> Env -> Env
execDo (i:is) env = exec (Do is) (exec i env)
execDo [] env = exec Nop env


-- !Q3 lexical analysis
data UOps = NotOp 
    deriving Show
data BOps = AddOp | SubOp | MulOp | DivOp | ModOp | ExpOp
    | AndOp | OrOp | EqOp | LtOp | LteOp
    deriving Show
data Keywords = IfK | ThenK | ElseK | WhileK | NopK
    deriving Show
data Token = VSym String | CSym Integer | BSym Bool
    | UOp UOps | BOp BOps | AssignOp
    | LPar | RPar | LBra | RBra | Semi
    | Keyword Keywords
    | Err String
    | PA AExpr | PB BExpr | PI Instr | Block [Instr]
    deriving Show


lexer :: String -> [Token]
lexer "" = []
--Keywords
lexer ('i':'f': s) = Keyword IfK : lexer s
lexer ('t':'h':'e':'n': s) = Keyword ThenK : lexer s 
lexer ('e':'l':'s':'e': s) = Keyword ElseK : lexer s 
lexer ('w':'h':'i':'l':'e': s) = Keyword WhileK : lexer s 
lexer ('n':'o':'p': s) = Keyword NopK : lexer s
--VSym / Variable
lexer (c:s) | isLower c =
    let (num, rst) = span (\x -> isAlphaNum x || x == '_') s
        in VSym (c:num) : lexer rst
--CSym / Constant
lexer (c:s) | isDigit c =
    let (num, rst) = span isDigit s
        in CSym(read(c:num)) : lexer rst
--BSym / Boolean
lexer ('T': s) = BSym True : lexer s
lexer ('F': s) = BSym False : lexer s
--UOp / NotOp
lexer ('!': s) = UOp NotOp : lexer s
--BOps (Boolean)
lexer ('/':'\\': s) = BOp AndOp : lexer s
lexer ('\\':'/': s) = BOp OrOp  : lexer s
lexer ('=':'=': s) = BOp EqOp : lexer s
lexer ('<':'=': s) = BOp LteOp : lexer s
lexer ('<': s) = BOp LtOp  : lexer s
--BOps (Arithmatic)
lexer ('+': s) = BOp AddOp : lexer s
lexer ('-': s) = BOp SubOp : lexer s
lexer ('*': s) = BOp MulOp : lexer s
lexer ('/': s) = BOp DivOp : lexer s
lexer ('^': s) = BOp ExpOp : lexer s
lexer ('%':s) = BOp ModOp : lexer s
--Assign
lexer (':':'=': s) = AssignOp : lexer s
--Puncuation
lexer ('(': s) = LPar : lexer s
lexer (')': s) = RPar : lexer s 
lexer ('{': s) = LBra : lexer s
lexer ('}': s) = RBra : lexer s 
lexer (';': s) = Semi : lexer s
--spaces
lexer (c: s) | isSpace c = lexer s
--err
lexer s = error ("Lexical error: " ++ s)



-- !Q4 parsing

parser :: [Token] -> [Instr]
parser ts = parserHelper (LBra : ts ++ [RBra])

parserHelper :: [Token] -> [Instr]
parserHelper ts = case sr [] ts of
    [PI e] -> [e]
    ts -> error ("Parse error: " ++ show ts) 


sr :: [Token] -> [Token] -> [Token]
--Vsym
sr (VSym x : s) ts = sr (PA(Var x) : s) ts
--CSym
sr (CSym x : s) ts = sr (PA (Const x) : s) ts
--BSym
sr (BSym x : s) ts = if x then sr (PB TT : s) ts else sr (PB FF : s) ts
--PA
sr (PA p2 : BOp AddOp : PA p1 : s) ts = sr (PA (Add p1 p2) : s) ts
sr (PA p2 : BOp SubOp : PA p1 : s) ts = sr (PA (Sub p1 p2) : s) ts
sr (PA p2 : BOp MulOp : PA p1 : s) ts = sr (PA (Mul p1 p2) : s) ts
sr (PA p2 : BOp DivOp : PA p1 : s) ts = sr (PA (Div p1 p2) : s) ts
sr (PA p2 : BOp ModOp : PA p1 : s) ts = sr (PA (Mod p1 p2) : s) ts
sr (PA p2 : BOp ExpOp : PA p1 : s) ts = sr (PA (Exp p1 p2) : s) ts
--PB
sr (PB p2 : BOp AndOp : PB p1 : s) ts = sr (PB (And p1 p2) : s) ts
sr (PB p2 : BOp OrOp : PB p1 : s) ts = sr (PB (Or p1 p2) : s) ts
sr (PA p2 : BOp EqOp : PA p1 : s) ts = sr (PB (Eq p1 p2) : s) ts
sr (PA p2 : BOp LtOp : PA p1 : s) ts = sr (PB (Lt p1 p2) : s) ts
sr (PA p2 : BOp LteOp : PA p1 : s) ts = sr (PB (Lte p1 p2) : s) ts
sr (PB p1 : UOp NotOp : s) ts = sr (PB (Not p1) : s) ts
--Pars
sr (RPar : PI e : LPar : s) ts = sr (PI e : s) ts
sr (RPar : PA e : LPar : s) ts = sr (PA e : s) ts
sr (RPar : PB e : LPar : s) ts = sr (PB e : s) ts
--Block
sr (Block is : LBra : s) ts = sr (PI (Do is): s) ts
sr (Block is : Semi : PI i : s) ts = sr (Block (i:is): s) ts
sr (RBra : PI i : s) ts = sr (Block [i] : s) ts
sr (RBra : s) ts = sr (Block []:s) ts
--keywords
sr (PA p : AssignOp : PA (Var x) : s) ts = sr (PI (Assign x p) : s) ts
sr (PI i : PB b : Keyword WhileK : s) ts = sr (PI (While b i) : s) ts
sr (PI i2 : Keyword ElseK : PI i1 : Keyword ThenK : PB b : Keyword IfK : s) ts = sr (PI (IfThenElse b i1 i2) : s) ts
sr (Keyword NopK : s) ts = sr (PI Nop : s) ts
--Empty
sr s (t:ts) = sr (t:s) ts
sr s [] = s


-- !Q5 I/O

main :: IO ()
main = do
    putStrLn "Enter a file to load" 
    fname <- getLine
    f <- readFile fname
    let pl = execList (parser (lexer f)) []
    print pl