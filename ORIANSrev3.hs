
--Bubble Sorting--

--1 done, add comments
bubble :: Ord a => [a] -> [a]
bubble [] = []
bubble [x] = [x]
bubble (x:y:xs)
    |   y <= x = y : bubble(x:xs)
    |   otherwise = x :bubble(y:xs)

--2 done, add comments
bubbleSort :: Ord a=> [a] -> [a]
bubbleSort [] = []
bubbleSort x = if new == x then x else bubbleSort new
    where
        new = bubble x

--Searching & Replacing Substrings--

--1 done
findString :: String -> String -> Bool
findString (_:_) []= False  --(_:_) doesnt matter what first input is, but ig 2nd is empty it is false
findString xs ys
    |   isPrefix xs ys = True
    |   findString xs (tail ys) = True  --tail takes everything but the first item, so the list shrinks
    |   otherwise = False

--helper function for #1 
isPrefix :: (Eq a) => [a] -> [a] -> Bool
isPrefix [] _ = True
isPrefix (_:_) [] = False
isPrefix (x:xs) (y:ys) = (x == y) && isPrefix xs ys

--2 done
genSubstrings :: String -> [String]
genSubstrings [] = []
genSubstrings x = removeDups(genPrefix(x) ++ genSubstrings(tail(x)))

--helper function for #2
genPrefix :: String -> [String]
genPrefix [] = [[]]
genPrefix (x:xs) = []: map (x:) (genPrefix xs)

--helper function for #2
removeDups :: (Eq a) => [a] -> [a]
removeDups [] = []
removeDups (x:xs) = if elem x ys then ys else x:ys
                    where ys = removeDups xs

--3 done
replacePrefix :: (String, String) -> String -> String
replacePrefix (_,_) [] = []
replacePrefix (old, new) str = if isPrefix old str then     --conditon
    new ++ replacePrefix(old, new) (drop(length old) str)    --removes the prefix 
    else head str : replacePrefix(old, new) (tail str)      --2nd time through no prefix so we come here and append, will do it multiple times

--4 almost done
replaceString :: (String, String) -> String -> String
replaceString (_,_) [] = [] 
replaceString (old, new) str = if findString old str then
    replaceString(old, new) (drop(length old) str) ++ new 
    else str
     

--A simple cypher--

--1 done
lookUp :: Char -> [(Char, Char)] -> Char
lookUp p [] = p
lookUp p ((x,y): ps)
    | x == p = y
    | otherwise = lookUp p ps

--2
--encode :: [(Char, Char)] -> String -> String
--encode [] _ = []
--encode [(_,_)] [] = []
--encode [(x, y)] str = 

--3
makeTable :: String -> String -> [(Char, Char)]
makeTable = error "error not finish"