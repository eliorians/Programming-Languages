import Data.Char

type Vars = String
data Prop = Var Vars | Const Bool | And Prop Prop | OR Prop Prop | Not Prop
            | Imp Prop Prop | Iff Prop Prop | Xor Prop Prop
    deriving (Show, Eq)
    --implication , equivalance, eXclusive OR

prop1 :: Prop
prop1 = Var "X" `And` Var "Y"               -- X /\ Y
prop2 :: Prop
prop2 = Var "X" `Imp` Var "Y"               -- X -> Y
prop3 :: Prop
prop3 = Not (Var "X") `OR` Var "Y"          -- !X \/ Y
prop4 :: Prop
prop4 = Not (Var "X") `Iff` Not (Var "Y")   -- !X <-> !Y


---------------------------


--2.1 Evaluator

type Env = [(Vars,Bool)]
eval :: Env -> Prop -> Bool
eval env (Var x) = case (lookup x env) of
    Nothing -> error $ "No value for variable " ++ x
    Just v  -> v
eval env (Const b) = b
eval env (And f1 f2) = eval env f1 && eval env f2
eval env (OR f1 f2)  = eval env f1 || eval env f2
eval env (Not f) = not (eval env f)
--eval env (Imp f1 f2) = 
--eval env (Iff f1 f2) = 
--eval env (Xor f1 f2) = 




--2.2 Lexical Analysis

-- binary operators
data BOps = AndOp | OrOp | ImpOp | IffOp | XorOp
    deriving (Show,Eq)
-- the type of tokens
data Token = VSym Vars | CSym Bool | BOp BOps | NotOp | LPar | RPar
            | PB Prop -- a token to store parsed boolean expressions
    deriving (Show,Eq)

lexer :: String -> [Token]
--Base case
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






--2.3 Parser
--parseProp :: [Token] -> Prop

--2.4 Searching for Truth Statment
--findSat :: Prop -> Maybe

--2.5 Putting it together
--solve :: String -> String