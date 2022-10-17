-- ":t function" shows type of "function"

--lambda = \ and the . == ->

-- x + y == (+) x y
-- div x y == x' div' y
-- $ 10 + 1 == (10 + 1)

-- I (Identity)
i :: a -> a
--i x = x
i = \x -> x

--K (Konstant)
k :: a -> b -> a
--k x y = x
k = const

--S (S-Combinator)
s :: (a -> b -> c) -> (a -> b) -> a -> c
s x y z = x z (y z)

--x is a function x::a -> b -- takes result of below and returns
--y is a function y:: c -> a --takes result of below and applies to x
--z is not a function z:: c  --which is applied to y
--B (Composition)
b :: (a->b) -> (c-> a) -> c -> b 
b x y z = x (y z)

--C (exChange)
c :: (a -> b -> c) -> (b -> a -> c)
c x y z = x z y

--(Pairing)

--(Duplication)

-- 1 (Application)
app :: (a -> b) -> a -> b
--app x y = x y
app = ($)
--(Numeral 2)

--(Owl)

--(Self Application)

--(Omega)
