import Data.Char

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
--tt
lexer ( 't' : 't' : s) = CSym True : lexer s
--ff
lexer ( 'f' : 'f' : s) = CSym False : lexer s
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


--2.4 Searching for Truth Statment done

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

evalAll :: Prop -> [Env] -> Maybe Env
evalAll p [] = Nothing
evalAll p (key:keys) = if eval key p then Just key else evalAll p keys

findSat :: Prop -> Maybe Env
findSat x = (evalAll x (genEnvs (fv x)))


--2.5 Putting it together done
solve :: String -> String
solve s = if (findSat(parseProp(lexer s)) == Nothing) then "No solution." else "Satisfiable."


--tests--


tests =
  [ ((eval [("E",True)] (And (And (Const True) (Imp (Var "E") (Var "E"))) (Const False))) == False)
  , ((eval [("E",True),("A",False)] (Not (Xor (Imp (Var "E") (Var "A")) (Var "A")))) == True)
  , ((eval [] (Const True)) == True)
  , ((eval [("P",False),("T",False)] (Xor (Iff (And (Var "P") (Var "P")) (Xor (Var "T") (Var "P"))) (Const False))) == True)
  , ((eval [("P",True),("T",True)] (Imp (And (Var "T") (Imp (Var "T") (Var "P"))) (Const True))) == True)
  , ((eval [("D",False)] (Not (Const False))) == True)
  , ((eval [("P",False),("T",True)] (Var "P")) == False)
  , ((eval [("A",True)] (Var "A")) == True)
  , ((eval [("X",True),("Y",False),("O",False),("E",False),("A",True),("D",False)] (And (Or (Or (Var "A") (Const False)) (Xor (Const False) (Const False))) (Var "X"))) == True)
  , ((eval [] (Const False)) == False)
  , ((lexer "((tt) /\\ ((Z) -> (D))) /\\ (ff)") == [LPar,LPar,CSym True,RPar,BOp AndOp,LPar,LPar,VSym "Z",RPar,BOp ImpOp,LPar,VSym "D",RPar,RPar,RPar,BOp AndOp,LPar,CSym False,RPar])
  , ((lexer "!(((B) -> (H)) <+> (Z))") == [NotOp,LPar,LPar,LPar,VSym "B",RPar,BOp ImpOp,LPar,VSym "H",RPar,RPar,BOp XorOp,LPar,VSym "Z",RPar,RPar])
  , ((lexer "tt") == [CSym True])
  , ((lexer "(((C) /\\ (V)) <-> ((R) <+> (X))) <+> (ff)") == [LPar,LPar,LPar,VSym "C",RPar,BOp AndOp,LPar,VSym "V",RPar,RPar,BOp IffOp,LPar,LPar,VSym "R",RPar,BOp XorOp,LPar,VSym "X",RPar,RPar,RPar,BOp XorOp,LPar,CSym False,RPar])
  , ((lexer "((O) /\\ ((D) -> (\1039))) -> (tt)") == [LPar,LPar,VSym "O",RPar,BOp AndOp,LPar,LPar,VSym "D",RPar,BOp ImpOp,LPar,VSym "\1039",RPar,RPar,RPar,BOp ImpOp,LPar,CSym True,RPar])
  , ((lexer "!(ff)") == [NotOp,LPar,CSym False,RPar])
  , ((lexer "J") == [VSym "J"])
  , ((lexer "Z") == [VSym "Z"])
  , ((lexer "(((G) \\/ (ff)) \\/ ((ff) <+> (ff))) /\\ (P)") == [LPar,LPar,LPar,VSym "G",RPar,BOp OrOp,LPar,CSym False,RPar,RPar,BOp OrOp,LPar,LPar,CSym False,RPar,BOp XorOp,LPar,CSym False,RPar,RPar,RPar,BOp AndOp,LPar,VSym "P",RPar])
  , ((lexer "ff") == [CSym False])
  , ((parseProp [LPar,LPar,CSym True,RPar,BOp AndOp,LPar,LPar,VSym "Z",RPar,BOp ImpOp,LPar,VSym "D",RPar,RPar,RPar,BOp AndOp,LPar,CSym False,RPar]) == And (And (Const True) (Imp (Var "Z") (Var "D"))) (Const False))
  , ((parseProp [NotOp,LPar,LPar,LPar,VSym "B",RPar,BOp ImpOp,LPar,VSym "H",RPar,RPar,BOp XorOp,LPar,VSym "Z",RPar,RPar]) == Not (Xor (Imp (Var "B") (Var "H")) (Var "Z")))
  , ((parseProp [CSym True]) == Const True)
  , ((parseProp [LPar,LPar,LPar,VSym "C",RPar,BOp AndOp,LPar,VSym "V",RPar,RPar,BOp IffOp,LPar,LPar,VSym "R",RPar,BOp XorOp,LPar,VSym "X",RPar,RPar,RPar,BOp XorOp,LPar,CSym False,RPar]) == Xor (Iff (And (Var "C") (Var "V")) (Xor (Var "R") (Var "X"))) (Const False))
  , ((parseProp [LPar,LPar,VSym "O",RPar,BOp AndOp,LPar,LPar,VSym "D",RPar,BOp ImpOp,LPar,VSym "\1039",RPar,RPar,RPar,BOp ImpOp,LPar,CSym True,RPar]) == Imp (And (Var "O") (Imp (Var "D") (Var "\1039"))) (Const True))
  , ((parseProp [NotOp,LPar,CSym False,RPar]) == Not (Const False))
  , ((parseProp [VSym "J"]) == Var "J")
  , ((parseProp [VSym "Z"]) == Var "Z")
  , ((parseProp [LPar,LPar,LPar,VSym "G",RPar,BOp OrOp,LPar,CSym False,RPar,RPar,BOp OrOp,LPar,LPar,CSym False,RPar,BOp XorOp,LPar,CSym False,RPar,RPar,RPar,BOp AndOp,LPar,VSym "P",RPar]) == And (Or (Or (Var "G") (Const False)) (Xor (Const False) (Const False))) (Var "P"))
  , ((parseProp [CSym False]) == Const False)
  , ((findSat (And (And (Const True) (Imp (Var "Z") (Var "D"))) (Const False))) == Nothing)
  , ((findSat (Not (Xor (Imp (Var "B") (Var "H")) (Var "Z")))) == Just [("B",True),("H",True),("Z",True)])
  , ((findSat (Const True)) == Just [])
  , ((findSat (Xor (Iff (And (Var "C") (Var "V")) (Xor (Var "R") (Var "X"))) (Const False))) == Just [("C",True),("V",True),("R",True),("X",False)])
  , ((findSat (Imp (And (Var "O") (Imp (Var "D") (Var "\1039"))) (Const True))) == Just [("O",True),("D",True),("\1039",True)])
  , ((findSat (Not (Const False))) == Just [])
  , ((findSat (Var "J")) == Just [("J",True)])
 , ((findSat (Var "Z")) == Just [("Z",True)])
  , ((findSat (And (Or (Or (Var "G") (Const False)) (Xor (Const False) (Const False))) (Var "P"))) == Just [("G",True),("P",True)])
  , ((findSat (Const False)) == Nothing)
  , ((solve "((tt) /\\ ((Z) -> (D))) /\\ (ff)") == "No solution.")
  , ((solve "!(((B) -> (H)) <+> (Z))") == "Satisfiable.")
  , ((solve "tt") == "Satisfiable.")
  , ((solve "(((C) /\\ (V)) <-> ((R) <+> (X))) <+> (ff)") == "Satisfiable.")
  , ((solve "((O) /\\ ((D) -> (\1039))) -> (tt)") == "Satisfiable.")
  , ((solve "!(ff)") == "Satisfiable.")
  , ((solve "J") == "Satisfiable.")
  , ((solve "Z") == "Satisfiable.")
  , ((solve "(((G) \\/ (ff)) \\/ ((ff) <+> (ff))) /\\ (P)") == "Satisfiable.")
  , ((solve "ff") == "No solution.")
  ]

main = putStrLn $ show (length (filter id tests)) ++ '/' : show (length tests)

getErrors = map fst . filter (not . snd) . zip [1..] $ tests