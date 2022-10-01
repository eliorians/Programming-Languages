
--Generating Strings--

--1
--mapAppend :: (a -> [a]) -> [a] -> [b]
--mapAppend =

--2 done
addLetter :: Char -> [String] -> [String]
addLetter x = map  ("x" ++)

--3
--addLetters :: [Char] -> [String] -> [String]


--4
--makeWords :: [Char] -> Integer -> [String]


--Propositional Logic--

type Vars = String
data Prop = Var Vars | Const Bool | And Prop Prop | Or Prop Prop | Not Prop
    deriving Show

prop1 = Var "X" `And` Var "Y"               -- X /\ Y
prop2 = Var "X" `Or` Var "Y"                -- X \/ Y
prop3 = Not (Var "X") `Or` (Var "Y")        -- !X \/ Y
prop4 = Not (Var "X") `Or` Not (Var "Y")    -- !X \/!Y

--1
--fv :: Prop -> [Vars]

--2
--lookUp :: Vars -> [(Vars,Bool)] -> Bool

--3
--eval :: [(Vars,Bool)] -> Prop -> Bool

--4
--evalAll :: Prop -> [ [ (Vars ,Bool) ] ] -> Bool

--5
--genEnvs :: [Vars] -> [[(Vars ,Bool)]]

--6
--sat :: Prop -> Bool