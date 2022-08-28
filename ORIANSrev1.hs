--1
radius :: Double -> Double -> Double
radius x y = sqrt((0-x)^2 + (0-y)^2)

--2
radius' :: (Double, Double) -> Double
radius' (x,y) = sqrt((0-x)^2 + (0-y)^2)

--3
sumEvens :: Integer -> Integer
sumEvens 0 = 0
sumEvens x = if even x then x + sumEvens(x-1) else sumEvens(x -1)

--4
sumEvens' :: Integer -> Integer
sumEvens x = let xl = [1..]

--5
collatz :: Integer -> Integer
collatz 0 = 1
collatz 1 = 1
collatz n = if even n then collatz(n/2)
collatz n = if odd n then collatz(3n+1)


--6

--7

--8

--9

--10
