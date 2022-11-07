
module Prop where
import Data.Char
import Data.List


-- 2 Read-eval-print loop
main :: IO ()
main = loop []

--Either a b = Left a | Right b
parseProp :: [Token] -> Either Prop String
parseProp ts = case sr [] ts of
  [PB e] -> Left e
  [Err m] -> Right m
  s      -> Right (error "Parse error" ++ show s)

parseDef :: [Token] -> Either (Name,Prop) String
parseDef  (NSym n : EqSym : ts) = case parseProp ts of
                            (Left p) -> Left (n,p)
                            Right e  -> Right e
parseDef inp = Right ("Error in definition syntax: " ++ show inp)


loop :: Def -> IO ()
loop def = do
  cmd <- getLine
  case words cmd of
    ("let" : s)    -> case parseDef (lexer (drop 3 cmd)) of 
                      Left (n,p) -> (putStrLn (show p)) >> loop ((n,p): def)
                      Right e    -> putStrLn e >> loop def
    ["print", f]   -> case lookup f def of
                      Just p  -> putStrLn (show p) >> loop def 
                      Nothing -> putStrLn ("Cannot find name " ++ f) >> loop def
    ["vars", f]    -> case lookup f def of
                    Just p  -> putStrLn (show (fv p)) >> loop def 
                    Nothing -> putStrLn ("Cannot find name " ++ f) >> loop def
    ["sat", f]     -> case lookup f def of
                    Just p  -> putStrLn (sat p) >> loop def 
                    Nothing -> putStrLn ("Cannot find name " ++ f) >> loop def
    ["tauto", f]   -> case lookup f def of
                    Just p  -> putStrLn (tauto p) >> loop def 
                    Nothing -> putStrLn ("Cannot find name " ++ f) >> loop def
    ["solve", f]   -> case lookup f def of
                    Just p  -> putStrLn (solve p) >> loop def 
                    Nothing -> putStrLn ("Cannot find name " ++ f) >> loop def
    ["eq", f, g]   -> case lookup f def of
                      Just p1 -> case lookup g def of
                        Just p2 -> if tauto(Iff p1 p2) == "Yes" then putStrLn "Yes" else putStrLn "No" >> loop def
                        Nothing -> putStrLn ("Cannot find name " ++ g) >> loop def
                      Nothing -> putStrLn ("Cannot find name " ++ f) >> loop def
    ["subst", x, "with", f, "in", g] -> case lookup f def of
          Just p1  -> case lookup g def of
                Just p2 -> putStrLn (show (replace x p1 p2)) >> loop def
                Nothing -> putStrLn ("Cannot find name " ++ g) >> loop def
          Nothing  -> putStrLn ("Cannot find name " ++ f) >> loop def
    ["load", f] -> do
      input <- readFile f
      let ls = lines input
      let pl = parseLines ls
      case pl of
        Left p -> loop p
        Right p -> putStrLn p >> loop def
    ["quit"] -> putStrLn "Bye!" >> return ()
    _ -> putStrLn "Unrecognized command." >> loop def


replace :: Vars -> Prop -> Prop -> Prop
replace x f (Var g) = if g == x then f else Var g
replace x f (Const g) = Const g
replace x f (And g1 g2) = And (replace x f g1) (replace x f g2)
replace x f (Or g1 g2) = Or (replace x f g1) (replace x f g2)
replace x f (Imp g1 g2) = Imp (replace x f g1) (replace x f g2)
replace x f (Iff g1 g2) = Iff (replace x f g1) (replace x f g2)
replace x f (Xor g1 g2) = Xor (replace x f g1) (replace x f g2)
replace x f (Not g) = Not (replace x f g)


--3 File I/O
parseLines :: [String] -> Either Def String
parseLines (s : lines) = case lexer s of
  (NSym v : EqSym : rst) -> case parseProp rst of 
                        Left p -> case parseLines lines of
                          Left p2  -> Left ((v, p) : p2)
                          Right e2 -> Right e2
                        Right e -> Right e
  e -> Right ("Error in parseLines " ++ show e)
parseLines [] = Left []


-- Propositional Logic setup
type Name = String
type Vars = String
data Prop = Var Vars | Const Bool | And Prop Prop | Or Prop Prop | Not Prop
          | Imp Prop Prop | Iff Prop Prop | Xor Prop Prop
  deriving (Show,Eq)

type Env = [(Vars,Bool)]
type Def = [(Name, Prop)]

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
           | Err String
           | NSym Name | EqSym 
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
lexer ('='         : s) = EqSym       : lexer s
lexer ('t':'t'     : s) = CSym True   : lexer s
lexer ('f':'f'     : s) = CSym False  : lexer s
lexer (c:s) | isUpper c = let (var,rst) = span isAlphaNum s
                           in VSym (c:var) : lexer rst
lexer (c:s) | isLower c = let (var,rst) = span isAlphaNum s
                           in NSym (c:var) : lexer rst
lexer (c:s) | isSpace c = lexer s
lexer s = error ("Lexical error: " ++ s)

-- 2.3 Parser

sr :: [Token] -> [Token] -> [Token]
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
sr (Err m : s)                       q = [Err m]
sr s                                [] = s

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

--previously called solve
sat :: Prop -> String
sat p = case (findSat p) of
              Nothing -> "No"
              Just _  -> "Yes"

--sat but you return the string
solve :: Prop -> String
solve p = case (findSat p) of
              Nothing -> "No Solution"
              Just x  -> show x


tauto :: Prop -> String
tauto p =  case (and [eval x p | x <- genEnvs (fv p)]) of
              True -> "Yes"
              False  -> "No"


--tests--
