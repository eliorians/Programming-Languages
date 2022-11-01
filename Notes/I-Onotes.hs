-- import Data.List
import Data.Char -- for isAlphaNum, isDigit, isSpace

type Vars = String
data AExpr = Var Vars | Const Integer | Add AExpr AExpr | Mul AExpr AExpr
           | Sub AExpr AExpr | Div AExpr AExpr
  deriving (Show,Eq)
data BinOps = PlusOp | MulOp | DivOp | SubOp | AssignOp
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
sr (RPar : PE e : LPar : s) q          = sr (PE e : s) q
sr s      (x:xs) = sr (x:s) xs
sr s      [] = error ("Parse error: " ++ show s)

lexer :: String -> [Token]
lexer "" = []
lexer ('(':s) = LPar : lexer s
lexer (')':s) = RPar : lexer s
lexer ('*':s) = BinOp MulOp : lexer s
lexer ('+':s) = BinOp PlusOp : lexer s
lexer ('/':s) = BinOp DivOp : lexer s
lexer ('-':s) = BinOp SubOp : lexer s
lexer (':':'=':s) = BinOp AssignOp : lexer s
lexer (c:s) | isDigit c =
  let (num,rst) = span isDigit s
   in CSym (read (c:num)) : lexer rst
lexer (c:s) | isAlpha c =
  let (num,rst) = span (\x -> isAlphaNum x || x == '_') s
   in VSym (c:num) : lexer rst
lexer (c:s) | isSpace c = lexer s
lexer s = error ("Lexical error: " ++ s)

parser :: String -> AExpr
parser = parseExpr . lexer

eval :: [(Vars,Integer)] -> AExpr -> Integer
eval env (Var v) = maybe (error ("Variable not found:" ++ v)) (id) (lookup v env)
-- eval env (Var v) = case lookup v env of
--                      Just n -> n
--                      Nothing -> error ("Variable not found:" ++ v)
eval env (Const n) = n
eval env (Add e1 e2) = eval env e1 + eval env e2
eval env (Mul e1 e2) = eval env e1 * eval env e2
eval env (Sub e1 e2) = eval env e1 - eval env e2
eval env (Div e1 e2) = eval env e1 `div` eval env e2

parseLine :: String -> (Vars,AExpr)
parseLine s = case lexer s of
  (VSym v : BinOp AssignOp : rst) -> (v, parseExpr rst)
  _ -> error $ "Cannot parse line:" ++ s

evalLines :: Env -> [(Vars,AExpr)] -> [(Vars,Integer)]
evalLines initEnv = evalHelper initEnv where
  evalHelper :: Env -> [(Vars,AExpr)] -> Env
  evalHelper env [] = env
  evalHelper env ((var,expr):lst) = evalHelper ((var,eval env expr):env) lst

-- REPL: Read-Eval-Print-Loop
type Env = [(Vars,Integer)]

loop :: Env -> IO ()
loop e = putStrLn "Enter a choice of eval, assign, or quit." >>
  getLine >>= (\s -> case words s of
  ["load",s] -> do
    input <- readFile s
    let ls = lines input
    let parsedLines = map parseLine ls
    let evaledLines = evalLines e parsedLines
    loop evaledLines
  ("eval":s') -> do
    -- putStrLn "Enter a string:"
    -- s <- getLine
    putStrLn "The result of lexer is:"
    let ls = lexer (unwords s')
    putStrLn (show ls)
    putStrLn "The result of parser is:"
    putStrLn (show (parseExpr ls))
    putStrLn "The result of evaluation is:"
    putStrLn (show (eval e (parseExpr ls)))
    loop e
  ["quit"] -> return ()
  ["assign"] -> do
    putStrLn "Enter a variable assignment in the form X := value"
    s <- getLine
    let (v,expr) = parseLine s
    loop ((v, eval e expr) : e)
  _ -> putStrLn "Input error!" >> loop e
  )

main :: IO ()
main = loop []
