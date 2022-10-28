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

--testing
main = do
  putStrLn . show $ (minList [0..5]) == 0
  putStrLn . show $ (minList [0]) == 0
  putStrLn . show $ (minList [7]) == 7
  putStrLn . show $ (minList [5, 3, 0, 4, 6, 10]) == 0
  putStrLn . show $ (minList [10, 4, 8, 2, 1]) == 1
  putStrLn . show $ (minList [100, 12, 33, 44, 89]) == 12
  putStrLn . show $ (addAbs [1, -2, 3, -4]) == 10
  putStrLn . show $ (addAbs [1, 10, -100]) == 111
  putStrLn . show $ (addAbs [0, 2, -5, 1]) == 8
  putStrLn . show $ (addAbs [-3, -4, -1, -8]) == 16
  putStrLn . show $ (addAbs [1, 3, 9, 10]) == 23
  putStrLn . show $ (addAbs [-1, 2, -3, 4]) == 10
  putStrLn . show $ (existsOdd [0,2..10]) == False
  putStrLn . show $ (existsOdd [1,3..9]) == True
  putStrLn . show $ (existsOdd [0, 2, 4, 9, 10]) == True
  putStrLn . show $ (existsOdd [1, 2, 4, 8, 10]) == True
  putStrLn . show $ (existsOdd [0, 2, 4, 6, 8, 11]) == True
  putStrLn . show $ (existsOdd [2, 34, 78, 89, 100]) == True
  putStrLn . show $ (findOdd [0,2..10]) == Nothing
  putStrLn . show $ (findOdd [1,3..9]) == Just 1
  putStrLn . show $ (findOdd [0, 2, 4, 9, 10]) == Just 9
  putStrLn . show $ (findOdd [1, 2, 4, 8, 10]) == Just 1
  putStrLn . show $ (findOdd [0, 2, 4, 6, 8, 11]) == Just 11
  putStrLn . show $ (findOdd [2, 34, 78, 89, 100]) == Just 89
  putStrLn . show $ (removeEmpty ["Hello", "there", "world"]) == ["Hello","there","world"]
  putStrLn . show $ (removeEmpty ["", "Hello", "world"]) == ["Hello","world"]
  putStrLn . show $ (removeEmpty ["Hello", "world", ""]) == ["Hello","world"]
  putStrLn . show $ (removeEmpty ["Hello", "", "there", "", "", "world", ""]) == ["Hello","there","world"]
  putStrLn . show $ (subtractEach [(5,1),(6,5),(7,10),(8,15),(9,20)]) == [4,1,-3,-7,-11]
  putStrLn . show $ (subtractEach [(0,0),(0,7),(8,3),(16,4),(25,25)]) == [0,-7,5,12,0]
  putStrLn . show $ (subtractEach [(-5,1),(6,-5),(8,-3),(-16,4),(-25,25)]) == [-6,11,11,-20,-50]
  putStrLn . show $ (makeGreeting (Just "")) == "Hello, !"
  putStrLn . show $ (makeGreeting (Just "Jesse")) == "Hello, Jesse!"
  putStrLn . show $ (makeGreeting (Just "Max")) == "Hello, Max!"
  putStrLn . show $ (makeGreeting (Just "Denzel")) == "Hello, Denzel!"
  putStrLn . show $ (makeGreeting Nothing) == "Hello!"
  putStrLn . show $ (catMaybes [Nothing, Just 1, Just 5, Nothing, Just 2]) == [1,5,2]
  putStrLn . show $ (catMaybes [Nothing, Nothing, Nothing, Just 4]) == [4]
  putStrLn . show $ (catMaybes [Just 5, Just 10, Just 9]) == [5,10,9]
  putStrLn . show $ (catMaybes [Nothing, Just 3, Just 1, Just 7]) == [3,1,7]
  putStrLn . show $ (catMaybes [Just 3, Just 1, Just 7, Nothing]) == [3,1,7]
  putStrLn . show $ (classify [Right 3, Right 5, Right 8, Left 'o', Right 0]) == ("o",[3,5,8,0])
  putStrLn . show $ (classify [Left 2, Left 3, Left 0, Left 9, Right "a"]) == ([2,3,0,9],["a"])
  putStrLn . show $ (classify [Right "Hi", Left 0, Right "there", Left 9, Left 10]) == ([0,9,10],["Hi","there"])
  putStrLn . show $ (classify [Left 'y', Right "hello", Right "world", Left 'e', Right "!", Left 's']) == ("yes",["hello","world","!"])
  putStrLn . show $ (isPrefix "hey" "hello") == False
  putStrLn . show $ (isPrefix "he" "hello") == True
  putStrLn . show $ (isPrefix [1,2] [1..5]) == True
  putStrLn . show $ (isPrefix [] [pi]) == True
  putStrLn . show $ (isPrefix [] [1..10]) == True
  putStrLn . show $ (isPrefix [1..10] []) == False
  putStrLn . show $ (isPrefix [1..5] [1..5]) == True
  putStrLn . show $ (isPrefix "" "hello") == True
  putStrLn . show $ (isPrefix "hey" "") == False
  putStrLn . show $ (isPrefix "hey" "he") == False