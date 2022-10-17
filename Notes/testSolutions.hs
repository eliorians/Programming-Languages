module Test1 where

-- Generating strings

mapAppend :: (a -> [b]) -> [a] -> [b]
mapAppend f l = foldr ((++).f) [] l

addLetter :: Char -> [String] -> [String]
addLetter x = map (x :)

addLetters :: [Char] -> [String] -> [String]
addLetters x s = mapAppend (flip addLetter s) x

makeWords :: [Char] -> Integer -> [String]
makeWords cs 0 = [""]
makeWords cs n = addLetters cs (makeWords cs (n-1))


-- Propositional Logic
type Vars = String
data Prop = Var Vars | Const Bool | And Prop Prop | Or Prop Prop | Not Prop
                     -- | Imp Prop Prop | Iff Prop Prop | Xor Prop Prop
  deriving Show

prop1 = Var "X" `And` Var "Y"                    --    X /\ Y
prop2 = Var "X" `Or`  Var "Y"                    --    X \/ Y
prop3 = Not (Var "X") `Or` (Var "Y")             --   !X \/ Y
prop4 = Not (Var "X") `Or` Not (Var "Y")         --   !X \/!Y

removeDups :: (Eq a) => [a] -> [a]
removeDups = foldr (\x -> (x :) . filter (/= x)) []

fv :: Prop -> [Vars]
fv (Var x) = [x]
fv (And f1 f2) = removeDups (fv f1 ++ fv f2)
fv (Or  f1 f2) = removeDups (fv f1 ++ fv f2)
fv (Not f)     = fv f
fv _ = []

lookUp :: Vars -> [(Vars,Bool)] -> Bool
lookUp x [] = error ("Cannot find variable " ++ x ++ " in the environment.")
lookUp x ((key,val):r) = if key == x then val else lookUp x r

eval :: [(Vars,Bool)] -> Prop -> Bool
eval env (Var x) = case (lookup x env) of
    Nothing -> error $ "No value for variable " ++ x
    Just v  -> v
eval env (Const b) = b
eval env (And f1 f2) = eval env f1 && eval env f2
eval env (Or f1 f2)  = eval env f1 || eval env f2
eval env (Not f) = not (eval env f)

evalAll :: Prop -> [[(Vars,Bool)]] -> Bool
evalAll = any . flip eval

genEnvs :: [Vars] -> [[(Vars,Bool)]]
genEnvs = foldr (\x y -> map ((x,True):) y ++ map ((x,False):) y) [[]]

sat :: Prop -> Bool
sat p = evalAll p (genEnvs (fv p))

evalAllTest = all id
  [ evalAll prop1 [[("X",False),("Y",True)]]                         == False
  , evalAll prop1 [[("X",False),("Y",True)],[("X",True),("Y",True)]] == True
  , evalAll prop2 [[("X",False),("Y",True)]]                         == True
  , evalAll prop2 []                                                 == False
  , evalAll prop4 [[("X",False),("Y",True)],[("X",True),("Y",True)]] == True
  , evalAll prop4 [[("X",True) ,("Y",True)]]                         == False
  ]

satTest = all id
  [ sat    (Var "X" `And` Var "Y")         == True
  , sat    (Var "X" `And` (Not $ Var "X")) == False
  ]