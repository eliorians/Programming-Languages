--questions
--curry/uncurry
-- ` `

--currying and uncurrying--

--1 done
mapPair :: (a -> b -> c) -> [(a,b)] -> [c]
mapPair = map . uncurry

--2 
--mapPair' :: (a -> b-> c) -> [(b,a)] -> [c]
--mapPair' = 

--zipWith--

--1 done
diff :: [Integer] -> [Integer] -> [Integer]
diff = zipWith (-)

--2 
--splice :: [String] -> [String] -> [String]
--splice =  

--map--

--1 done
sqLens :: [String] -> [Integer]
sqLens = map ((^2) . fromIntegral . length)

--2 done
bang :: [String] -> [String]
bang = map (++ "!")


--filter--

--1 done
digitsOnly :: [Integer] -> [Integer]
digitsOnly = filter (\x -> (x > 0 ) && (x < 10))

--2 done
removeXs :: [String] -> [String]
removeXs = filter (notElem 'X')

--folds--

--1 recursive done
findNum :: Integer -> [Integer] -> Bool
findNum y [] = False
findNum y (x:xs) = if y == x then True else findNum y xs

--1 fold done
findNum' :: Integer -> [Integer] -> Bool
findNum' y = foldr (\ x -> (||) (y == x)) False

--2 recursive
--exists :: (a -> Bool) -> [a] -> Bool
--exists =

--2 fold
--exists' :: (a -> Bool) -> [a] -> Bool
--exists' =

--3 recursive done
noDups :: Eq a => [a] -> [a]
noDups [] = []
noDups (x:xs) = if x `elem` xs then noDups xs else x : noDups xs

--3 fold
--noDups' :: Eq a => [a] -> [a]
--noDups' (x:xs) = 

--4 recursive
--countOverflow :: Integer -> [String] -> Integer
--countOVerflow y [] = 0
--countOverflow y (x:xs) =

--4 fold
--countOverflow' :: Integer -> [String] -> Integer
--countOverflow' x xs = 

--5 recursive
--concatList :: [[a]] -> [a]
--concatList = 

--5 fold
--concatList :: [[a]] -> [a]
--concatList = 

--6 recursive
--bindList :: (a -> [b]) -> [a] -> [b]
--bindList = 

--6 fold
--bindList :: (a -> [b]) -> [a] -> [b]
--bindList =