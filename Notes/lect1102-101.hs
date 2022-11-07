-- import Data.List
import Data.Char -- for isAlphaNum, isDigit, isSpace

type Vars = String
type Value = Integer
type Name = String

data FunDef = FunDef { name :: Name
                     , vars :: [Vars]
                     , body :: AExpr }

data AExpr = Var Vars | Const Integer | Add AExpr AExpr | Mul AExpr AExpr
           | Sub AExpr AExpr | Div AExpr AExpr
           | FCall Name [AExpr]
           | IfZero AExpr AExpr AExpr
  deriving (Show,Eq)

type Env = [(Vars,Value)]
type Defs = [FunDef]

lookupDef :: Name -> Defs -> Maybe FunDef
lookupDef n [] = Nothing
-- lookupDef n ((FunDef name vars body) : ds) | n == name = Just (FunDef name vars body)
-- lookupDef n (fd@(FunDef name vars body) : ds) | n == name = Just fd
lookupDef n (fd : ds) | n == name fd = Just fd
lookupDef n (fd : ds) = lookupDef n ds

eval :: Defs -> Env -> AExpr -> Value
eval defs env (Var v) = maybe (error ("Variable not found:" ++ v)) (id) (lookup v env)
eval defs env (Const n) = n
eval defs env (Add e1 e2) = eval defs env e1 + eval defs env e2
eval defs env (Mul e1 e2) = eval defs env e1 * eval defs env e2
eval defs env (Sub e1 e2) = eval defs env e1 - eval defs env e2
eval defs env (Div e1 e2) = ede e1 `div` ede e2 where ede = eval defs env
eval defs env (IfZero e1 e2 e3) = if ede e1 == 0 then ede e2 else ede e3
                                  where ede = eval defs env
eval defs env (FCall fn args) =
  case lookupDef fn defs of
    Just (FunDef name vars body) -> eval defs newenv body
      where newenv = zip vars (map (eval defs env) args)
    Nothing -> error ("Could not find function named" ++ fn)

data BinOps = PlusOp | MulOp | DivOp | SubOp
            | AndOp
  deriving (Show,Eq)
data Token = BinOp BinOps | CSym Integer | VSym String | LPar | RPar | PE AExpr
           | EqSym | Comma | NSym Name | IfZeroK
           | ArgBlock [AExpr]
  deriving (Show,Eq)

isVar :: Token -> Bool
isVar (VSym _) = True
isVar _        = False

unVar :: Token -> String
unVar (VSym v) = v

parseDef :: [Token] -> FunDef
parseDef (NSym n : ts) = case span isVar ts of
  (vars, EqSym : rst) -> FunDef n (map unVar vars) (parseExpr rst)
  _                   -> error "Error in definition syntax"


parseExpr :: [Token] -> AExpr
parseExpr ts = case (sr [] ts) of
  [PE e] -> e
  ts -> error ("Parse error: " ++ show ts)

sr :: [Token] -> [Token] -> [Token]
sr (VSym x : s) q = sr (PE (Var x) : s) q   --R1
sr (CSym c : s) q = sr (PE (Const c) : s) q --R2
sr (PE e2 : BinOp PlusOp : PE e1 : s) q = sr (PE (Add e1 e2) : s) q --R3
sr (PE e2 : BinOp MulOp : PE e1 : s) q = sr (PE (Mul e1 e2) : s) q --R3
sr (PE e2 : BinOp SubOp : PE e1 : s) q = sr (PE (Sub e1 e2) : s) q --R3
sr (PE e2 : BinOp DivOp : PE e1 : s) q = sr (PE (Div e1 e2) : s) q --R3
sr (RPar : PE e : LPar : s) q          = sr (PE e : s) q
sr (RPar : PE e3 : Comma : PE e2 : Comma : PE e1 : LPar : IfZeroK : s) q
                                       = sr (PE (IfZero e1 e2 e3) : s) q
sr (LPar : NSym fn : s)              q = sr (ArgBlock [] : NSym fn : s) q
sr (Comma : PE e : ArgBlock args : s)q = sr (ArgBlock (e:args) : s) q
sr (RPar : PE e : ArgBlock args : NSym fn: s) q = sr (PE (FCall fn (reverse (e:args))) : s) q
sr (RPar : ArgBlock args : NSym fn : s) q = sr (PE (FCall fn (reverse args)) : s) q
sr s      (x:xs) = sr (x:s) xs
sr s [] = s

lexer :: String -> [Token]
lexer "" = []
lexer ('=':s) = EqSym : lexer s
lexer (',':s) = Comma : lexer s
lexer ('(':s) = LPar : lexer s
lexer (')':s) = RPar : lexer s
lexer ('*':s) = BinOp MulOp : lexer s
lexer ('+':s) = BinOp PlusOp : lexer s
lexer ('/':s) = BinOp DivOp : lexer s
lexer ('-':s) = BinOp SubOp : lexer s
lexer s | take 6 s == "ifZero" = IfZeroK : lexer (drop 6 s)
lexer (c:s) | isDigit c =
  let (num,rst) = span isDigit s
   in CSym (read (c:num)) : lexer rst
lexer (c:s) | isLower c =
  let (num,rst) = span (\x -> isAlphaNum x || x == '_') s
   in VSym (c:num) : lexer rst
lexer (c:s) | isUpper c =
  let (num,rst) = span (\x -> isAlphaNum x || x == '_') s
   in NSym (c:num) : lexer rst
lexer (c:s) | isSpace c = lexer s
lexer s = error ("Lexical error: " ++ s)

parser :: String -> AExpr
parser = parseExpr . lexer


-- REPL: Read-Eval-Print-Loop
update :: Vars -> Value -> Env -> Env
update x v [] = [(x,v)]
update x v ((y,w):es) | x == y = (x,v) : es
update x v ((y,w):es) | x /= y = (y,w) : update x v es

loop :: Defs -> IO ()
loop ds = putStrLn "Enter a choice of load, eval, define, or quit." >>
  getLine >>= (\s -> case words s of
  ["load",s] -> do
    input <- readFile s
    let ls = lines input
    let parsedLines = map (parseDef . lexer) ls -- :: Defs
    loop parsedLines
  ("eval":s') -> do
    -- putStrLn "Enter a string:"
    -- s <- getLine
    putStrLn "The result of lexer is:"
    let ls = lexer (unwords s')
    putStrLn (show ls)
    putStrLn "The result of parser is:"
    putStrLn (show (parseExpr ls))
    putStrLn "The result of evaluation is:"
    putStrLn (show (eval ds [] (parseExpr ls)))
    loop ds
  ["quit"] -> return ()
  ["define"] -> do
    putStrLn "Enter a function in the form Fname x1 .. xk = body"
    s <- getLine
    let fd = parseDef (lexer s)
    loop (fd : ds)
  _ -> putStrLn "Input error!" >> loop ds
  )

main :: IO ()
main = loop []
