
data Nat = Zero | Succ Nat
    deriving Show


expt :: Nat -> Nat -> Nat
expt x y = recNat y (const Succ) x

--given--

recNat :: a -> (Nat -> a -> a) -> Nat -> a
recNat z f Zero     = z
recNat z f (Succ n) = f n (recNat z f n)

add :: Nat -> Nat -> Nat
add x y = recNat y (const Succ) x

mul :: Nat -> Nat -> Nat
mul x y = recNat y (const Succ) x

--testing--

nat2int :: Nat -> Integer
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Integer -> Nat
int2nat x | x <= 0 = Zero
int2nat x          = Succ (int2nat(x-1))

testfun :: (Nat -> Nat -> Nat) -> Integer -> Integer -> Integer
testfun f x y = nat2int (f (int2nat x) (int2nat y))