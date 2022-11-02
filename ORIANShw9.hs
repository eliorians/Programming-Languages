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


-- update (x,v) e sets the value of x to v and keeps other variables in e the same
update :: (Vars, Integer) -> Env -> Env
update (x, v) e = if lookup x e then (x, v) else (x, v)

--Q1 eval arthimitic and boolean expressions
--evala :: Env -> AExpr -> Integer
--evalb :: Env -> BExpr -> Bool

--Q2 execute instructions

--Q3 lexical analysis

--Q4 parsing

--Q5 I/O