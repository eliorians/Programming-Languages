
--currying and uncurrying--

--1 done
mapPair :: (a -> b -> c) -> [(a,b)] -> [c]
mapPair = map . uncurry

--2 done
mapPair' :: (a -> b-> c) -> [(b,a)] -> [c]
mapPair' = map . uncurry'

uncurry' :: (a -> b -> c) ->(b,a) -> c
uncurry' f (b,a) = f a b

--zipWith--

--1 done
diff :: [Integer] -> [Integer] -> [Integer]
diff = zipWith (-)

--2 done but make it into a lambda?
splice :: [String] -> [String] -> [String]
splice = zipWith (\a b -> a ++ b ++ a)

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
digitsOnly = filter (\x -> (x >= 0 ) && (x < 10))

--2 MUST ONLY FILTER STRINGS THAT BEGIN WITH X
removeXs :: [String] -> [String]
removeXs = filter (notElem 'X')

--folds--

--1 recursive done
findNum :: Integer -> [Integer] -> Bool
findNum y [] = False
findNum y (x:xs) = (y == x) || findNum y xs

--1 fold done
findNum' :: Integer -> [Integer] -> Bool
findNum' y = foldr (\x -> (||) (y == x)) False

--2 recursive done
exists :: (a -> Bool) -> [a] -> Bool
exists f [] = False
exists f (x:xs) = f x || exists f xs

--2 fold done
exists' :: (a -> Bool) -> [a] -> Bool
exists' f = foldl (\acc x -> f x || acc) False

--3 recursive done
noDups :: Eq a => [a] -> [a]
noDups [] = []
noDups (x:xs) = if x `elem` xs then noDups xs else x : noDups xs

--3 fold done
noDups' ::  Eq a => [a] -> [a]
noDups' = foldl (\dup x -> if x `elem` dup then dup else dup ++ [x]) []

--4 recursive done
countOverflow :: Integer -> [String] -> Integer
countOverflow y [] = 0
countOverflow y (xs:xss) = if y < fromIntegral (length xs) then 1 + countOverflow y xss else countOverflow y xss

--4 fold done
countOverflow' :: Integer -> [String] -> Integer
countOverflow' y = foldl (\x xs-> if y < fromIntegral (length xs) then 1 + x else x) 0

--5 recursive done
concatList :: [[a]] -> [a]
concatList [] = []
concatList (xs:xss) = xs ++ concatList xss

--5 fold done
concatList' :: [[a]] -> [a]
concatList' = foldr (++) []

--6 recursive done
bindList :: (a -> [b]) -> [a] -> [b]
bindList f [] = []
bindList f (xs:xss) = (f xs) ++ bindList f xss

--6 fold done
bindList' :: (a -> [b]) -> [a] -> [b]
bindList' f = foldr ((++) . f) []
