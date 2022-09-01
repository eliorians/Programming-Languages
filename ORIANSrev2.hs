--1 DONE
minList :: [Integer] -> Integer
minList [] = 0
minList [x] = x
minList (x:xs) =  min x (minList xs)

--2 DONE
addAbs :: [Integer] -> Integer
addAbs = sum . map abs

--3 DONE
existsOdd :: [Integer] -> Bool
existsOdd [] = False
existsOdd (x:xs) = if odd x then True else existsOdd xs

--4 DONE
findOdd :: [Integer] -> Maybe Integer
findOdd [] = Nothing
findOdd (x:xs) = if odd x then Just x else findOdd xs

--5 DONE
removeEmpty :: [String] -> [String]
removeEmpty = filter (not . null)

--6 DONE
subtractEach :: [(Integer, Integer)] -> [Integer]
subtractEach [] = []
subtractEach  ((x,y):ps) = (x - y) : subtractEach ps

--7 DONE
makeGreeting :: Maybe String -> String
makeGreeting Nothing = "Hello!"
makeGreeting (Just x) = "Hello, " ++ x ++ "!"

--8 DONE
catMaybes :: [Maybe a] -> [a]
catMaybes ls = [x | Just x <- ls]

--9 DONE
classify :: [Either a b] -> ([a], [b])
classify [] = ([],[])
classify xs = (leftList xs, rightList xs)

leftList :: [Either a b] -> [a]
leftList [] = []
leftList (Left x:xs) = x : leftList xs
leftList (Right x:xs) = leftList xs

rightList :: [Either a b] -> [b]
rightList [] = []
rightList (Right x:xs) = x : rightList xs
rightList (Left x:xs) = rightList xs

--10 DONE
isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix a b = take 1 a == take 1 b