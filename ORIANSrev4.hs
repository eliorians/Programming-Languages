

--currying and uncurrying--

--1 done
mapPair :: (a -> b -> c) -> [(a,b)] -> [c]
mapPair = map . uncurry

--2 
--mapPair' :: (a -> b-> c) -> [(b,a)] -> [c]
--mapPair' = map . curry

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

--1
--2
--3
--4
--5
--6