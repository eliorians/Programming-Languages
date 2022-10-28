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

--testing
main = do
  putStrLn . show $ (radius 0.0 0.0) == 0.0
  putStrLn . show $ (radius 0.5 0.5) == 0.7071067811865476
  putStrLn . show $ (radius 1.0 1.0) == 1.4142135623730951
  putStrLn . show $ (radius 2.5 3.0) == 3.905124837953327
  putStrLn . show $ (radius (-1.0) (-1.0)) == 1.4142135623730951
  putStrLn . show $ (radius (-1.0) (1.0)) == 1.4142135623730951
  putStrLn . show $ (radius (-2.5) (-3.0)) == 3.905124837953327
  putStrLn . show $ (radius (-2.5) (3.0)) == 3.905124837953327
  putStrLn . show $ (radius' (0.0, 0.0)) == 0.0
  putStrLn . show $ (radius' (0.5, 0.5)) == 0.7071067811865476
  putStrLn . show $ (radius' (1.0, 1.0)) == 1.4142135623730951
  putStrLn . show $ (radius' (2.5, 3.0)) == 3.905124837953327
  putStrLn . show $ (radius' ((-1.0), (-1.0))) == 1.4142135623730951
  putStrLn . show $ (radius' ((-1.0), (1.0))) == 1.4142135623730951
  putStrLn . show $ (radius' ((-2.5), (-3.0))) == 3.905124837953327
  putStrLn . show $ (radius' ((-2.5), (3.0))) == 3.905124837953327
  putStrLn . show $ (sumEvens 0) == 0
  putStrLn . show $ (sumEvens 5) == 6
  putStrLn . show $ (sumEvens 8) == 20
  putStrLn . show $ (sumEvens 12) == 42
  putStrLn . show $ (sumEvens 15) == 56
  putStrLn . show $ (sumEvens' 0) == 0
  putStrLn . show $ (sumEvens' 5) == 6
  putStrLn . show $ (sumEvens' 8) == 20
  putStrLn . show $ (sumEvens' 12) == 42
  putStrLn . show $ (sumEvens' 15) == 56
  putStrLn . show $ (collatz 0) == 1
  putStrLn . show $ (collatz 1) == 1
  putStrLn . show $ (collatz 5) == 1
  putStrLn . show $ (collatz 10) == 1
  putStrLn . show $ (collatz 100) == 1
  putStrLn . show $ (init' [0]) == []
  putStrLn . show $ (init' [7]) == []
  putStrLn . show $ (init' [0..4]) == [0,1,2,3]
  putStrLn . show $ (init' [0..5]) == [0,1,2,3,4]
  putStrLn . show $ (init' [3, 9, 0, 1, 3, 4]) == [3,9,0,1,3]
  putStrLn . show $ (init' [2, 4, 9, 5, 7]) == [2,4,9,5]
  putStrLn . show $ (findEmpty ["hello", "world", "testing", ""]) == True
  putStrLn . show $ (findEmpty ["", "hello", "world", "testing"]) == True
  putStrLn . show $ (findEmpty ["hello", "world", "", "testing"]) == True
  putStrLn . show $ (findEmpty ["", "", "", ""]) == True
  putStrLn . show $ (getLengths ["hello", "world"]) == [5,5]
  putStrLn . show $ (getLengths ["", ""]) == [0,0]
  putStrLn . show $ (getLengths ["", "hello world"]) == [0,11]
  putStrLn . show $ (getLengths ["functional", "is", "fun"]) == [10,2,3]