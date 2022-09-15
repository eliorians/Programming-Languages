--currying and uncurrying--

--1 
mapPair :: (a -> b-> c) -> [(a,b)] -> [c]
mapPair f [] = []
mapPair f ((x,y) : ps) = f x y : mapPair f ps 

--2
mapPair' :: (a -> b-> c) -> [(b,a)] -> [c]
mapPair' f [] = []
mapPair' f ((x,y) : ps) = f y x : mapPair' f ps

--zipWith--

--1


--2

--map--

--1
--2

--filter--

--1
--2

--folds--
--1
--2
--3
--4
--5
--6