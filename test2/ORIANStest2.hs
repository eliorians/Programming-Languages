
import Data.Char
import Data.List

-- #1 Type Consctuctor --

-- T ::= Z | T -> T
data Types = Ints | Fun Types Types

-- /\ ::= \/ | /\ /\ | \ \/ /\ | C Int | /\ + /\ | IfZero(/\,/\,/\) | Y
data Terms a = Var a | App (Terms a) (Terms a) | Abs (Terms (Maybe a)) | Const Integer
                     | Add (Terms a) (Terms a) | IfZero (Terms a) (Terms a) (Terms a) | Y
                     deriving (Show, Eq)

unitTerms :: a -> Terms a
unitTerms = Var

bindTerms :: (a -> Terms b) -> Terms a -> Terms b
bindTerms f Y = Y
bindTerms f (Const x) = Const x
bindTerms f (Var x) = f x
bindTerms f (Abs x) = Abs (bindTerms (liftTerms f) x)
bindTerms f (App x y) = App (bindTerms f x) (bindTerms f y)
bindTerms f (Add x y) = Add (bindTerms f x) (bindTerms f y)
bindTerms f (IfZero x y z) = IfZero (bindTerms f x) (bindTerms f y) (bindTerms f z)

liftTerms :: (a -> Terms b) -> Maybe a -> Terms (Maybe b)
liftTerms f (Just a)  = fmap Just (f a)
liftTerms f Nothing = Var Nothing

instance Functor Terms where
    fmap f = bindTerms (unitTerms . f)

instance Applicative Terms where
    pure    = unitTerms
    f <*> t = bindTerms (<$> t) f

instance Monad Terms where
    return  = unitTerms
    t >>= f = bindTerms f t


-- #2 Parsing & Lexical Analysis --

data Token = VSym String | CSym Integer | AddOp | IfZeroOp | YComb
            | LPar | RPar | Dot | Comma | Backslash 
            | Err String | PT (Terms String)
            deriving (Eq, Show)


lexer :: String -> [Token]
lexer "" = []
lexer (c: s) | isSpace c = lexer s
--VSym
lexer (c:s) | isLower c =
    let (num, rst) = span (\x -> isAlphaNum x || x == '_') s
        in VSym (c:num) : lexer rst
--CSym
lexer (c:s) | isDigit c =
    let (num, rst) = span isDigit s
        in CSym(read(c:num)) : lexer rst
--Negative CSym
lexer ('-': c :s) = 
    let (num, rst) = span isDigit s
        in CSym(read(c:num)) : lexer rst
--Ops
lexer ('+': s) = AddOp : lexer s
lexer ('I':'f':'Z':'e':'r':'o': s) = IfZeroOp : lexer s
lexer ('Y': s) = YComb : lexer s
--Puncuation
lexer ('(': s) = LPar : lexer s
lexer (')': s) = RPar : lexer s 
lexer ('.': s) = Dot : lexer s 
lexer (',': s) = Comma : lexer s 
lexer ('\\': s) = Backslash : lexer s 
--Error
lexer s = error ("Lexical error: " ++ s)


parser :: [Token] -> Either (Terms String) String
parser ts = case sr [] ts of
    [ PT t ] -> Left t
    [ Err s] -> Right ("Lexical error: " ++ s)
    s        -> Right ("Parse error: " ++ show s)


sr :: [Token] -> [Token] -> [Token]
--VSym
sr (VSym x : s) q = sr (PT (Var x) : s) q
--CSym
sr (CSym x : s) q = sr (PT (Const x) : s) q
--Ops
sr (RPar : PT e1 : LPar : YComb : s) q = sr (PT e1 : PT Y : s) q
sr (PT e2 : AddOp : PT e1 : s) q = sr (PT (Add e1 e2) : s) q
sr (PT e2 : PT e1 : s) q = sr (PT (App e1 e2) : s) q
sr (PT e : Dot : PT(Var x) : Backslash : s) q = sr (PT (Abs (capture x e)) : s) q
sr (RPar : PT e3 : Comma : PT e2 : Comma : PT e1 : LPar : IfZeroOp : s) q = sr (PT (IfZero e1 e2 e3) : s) q
--Punctuation
sr (RPar : PT e : LPar : s) q = sr (PT e : s) q
--Error
sr (Err m : s) q = [Err m]
sr s (x:q) = sr (x:s) q
sr s [] = s


capture :: String -> Terms String -> Terms (Maybe String)
capture x s = s >>= (\y -> if x==y then Var Nothing else Var (Just y))


-- #3 Reduction & Evaluation --

subst :: Terms (Maybe a) -> Terms a -> Terms a
subst s t = s >>= maybe t Var

predstep :: Terms a -> Terms a
predstep (App Y x) = App x (App Y x)
predstep (App (Abs x) t) = subst x t
predstep (Add (Const x) (Const y)) = Const (x+y)
predstep (IfZero (Const 0) y z) = predstep y
predstep (IfZero (Const x) y z) = predstep z
--predstep (IfZero x y z) = IfZero (predstep x) (predstep y) (predstep z)
predstep t = t

preds :: Eq a => Terms a -> Terms a
preds t = case predstep t of
  (App Y t) -> App t (App Y t)
  (Var x) -> Var x
  (Const x) -> Const x
  (Abs x) -> Abs (preds x)
  (Add x y) -> Add (preds x) (preds y)
  (App x y) -> App (preds x) (preds y)
  (IfZero (Const 0) y z) -> preds y
  (IfZero (Const x) y z) -> preds z
  --(IfZero x y z) -> IfZero (preds x) (preds y) (preds z)
  t -> preds t


-- Testing --

-- a pretty-printer for Terms String
showTerm :: Terms String -> String
showTerm = st 0
    where -- st d (Var "o") = "v0"
        st d (Var x) = let (h,v) = span (== '|') x
                           l     = length h + 1
                        in if l <= d then v ++ show (d - l) else drop l x
        st d (Const n) = show n
        st d (Y) = "Y"
        st d (Add s t) = "(" ++ st d s ++ " + " ++ st d t ++ ")"
        st d (IfZero r s t) = "IfZero(" ++ st d r ++","++ st d s ++","++ st d t ++ ")"
        st d (Abs r) = '\\' : 'v' : show d ++ "." ++ st (d+1) (maybe "v" ('|':) <$> r)
        st d (App s t@(App _ _)) = st d s ++ "(" ++ st d t ++ ")"
        st d (App s t@(Abs _)) = st d s ++ "(" ++ st d t ++ ")"
        st d (App s t) = st d s ++ st d t

printTerm :: Terms String -> IO ()
printTerm = putStrLn . showTerm

readTerm :: String -> Terms String
readTerm s = case parser (lexer s) of
    Left t -> t
    Right e -> error $ "No parse: " ++ e

eval :: String -> Terms String
eval = preds . readTerm

($$) :: Terms String -> Integer -> Terms String
t $$ n = App t (Const n)

-- subtract 1
sub1 = "\\n.(n+(-1))"
-- multiply two inputs
mul = "Y(\\f.\\m.\\n.IfZero(m,0,n+(f(m+-1)n)))"
-- factorial
fac = "Y(\\f.\\x.IfZero(x,1," ++ '(' : mul ++ ")x(f(x+-1))))"
mult = readTerm mul -- parse of mul
fact = readTerm fac -- parse of fac
threeTimesTwenty = (printTerm . preds) (mult $$ 3 $$ 20)
fiveFactorial = (printTerm . preds) (fact $$ 5)

main :: IO ()
main = do
    putStrLn "Enter a PCF term:"
    s <- getLine
    let repl t = do
        putStrLn "Enter a command"
        i <- getLine
        case i of
            "lex" -> putStrLn (show (lexer s)) >> repl t
            -- "parse" -> putStrLn (showTerm (readTerm s)) >> repl t
            "red" -> putStrLn (showTerm t') >> repl t' where t' = predstep t
            "norm" -> putStrLn (showTerm t') >> repl t where t' = preds t
            "show" -> putStrLn (showTerm t) >> repl t
            "quit" -> return ()
            "new" -> main
    repl (readTerm s)
