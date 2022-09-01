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

--9
classify :: [Either a b] -> ([a], [b])
classify [] = ([],[])
classify (Left x:zs) = x : classify zs
classify (Right y:zs) = y : classify zs

--10 DONE
isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix a b = take 1 a == take 1 b