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
sumEvens' x = sum (filter even [1..x])

--5
collatz :: Integer -> Integer
collatz 0 = 1
collatz 1 = 1
collatz x = if even x then collatz(div 2 x) else collatz(3*x+1)

--6
collatzCheck :: [Integer]
collatzCheck = map collatz[1..100]

--7
multiplesOfFive :: [Integer]
multiplesOfFive = [5, 10..100]

--8
init' :: [Integer] -> [Integer]
init' = reverse . tail . reverse

--9
findEmpty :: [String]->Bool
findEmpty = all null

--10
getLengths :: [String] -> [Int]
getLengths = map length