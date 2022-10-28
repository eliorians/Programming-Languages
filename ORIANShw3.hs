
--Bubble Sorting--

--1 done
bubble :: Ord a => [a] -> [a]
bubble [] = []
bubble [x] = [x]
bubble (x:y:xs)
    |   y <= x = y : bubble(x:xs)
    |   otherwise = x :bubble(y:xs)

--2 done
bubbleSort :: Ord a=> [a] -> [a]
bubbleSort [] = []
bubbleSort x = if new == x then x else bubbleSort new
    where
        new = bubble x

--Searching & Replacing Substrings--

--1 done
findString :: String -> String -> Bool
findString (_:_) [] = False  --(_:_) doesnt matter what first input is, but if 2nd is empty it is false
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

--4 done
replaceString :: (String, String) -> String -> String
replaceString (_,_) [] = []
replaceString (old, new) str = if isPrefix old str then
    new ++ replacePrefix(old, new) (drop(length old) str)
    else head str : replaceString(old, new) (tail str)

--A simple cypher--

--1 done
lookUp :: Char -> [(Char, Char)] -> Char
lookUp p [] = p
lookUp p ((x,y): ps)
    | x == p = y
    | otherwise = lookUp p ps

--2 done
encode :: [(Char, Char)] -> String -> String
encode xs [] = []
encode [] (z:zs) = zs
encode xs (z:zs) = lookUp z xs : encode xs zs

--3
makeTable :: String -> String -> [(Char, Char)]
makeTable = zip


--testing
tests =
  [ ((bubble [0..5]) == [0,1,2,3,4,5])
  , ((bubble [1,0,2,5,3,4]) == [0,1,2,3,4,5])
  , ((bubble [5,4,3,2,1,0]) == [4,3,2,1,0,5])
  , ((bubble [10]) == [10])
  , ((bubble ['a']) == "a")
  , ((bubbleSort [0..5]) == [0,1,2,3,4,5])
  , ((bubbleSort [1,0,2,5,3,4]) == [0,1,2,3,4,5])
  , ((bubbleSort [5,4,3,2,1,0]) == [0,1,2,3,4,5])
  , ((bubbleSort [10]) == [10])
  , ((bubbleSort ['a']) == "a")
  , ((findString "" "hello") == True)
  , ((findString "hey" "") == False)
  , ((findString "hello" "hello world") == True)
  , ((findString "de" "abcdef") == True)
  , ((findString "hey" "hello world") == False)
  , ((genSubstrings "") `setEq` [""])
  , ((genSubstrings "abcd") `setEq` ["a","ab","abc","abcd","b","bc","bcd","c","cd","d",""])
  , ((genSubstrings "hello") `setEq` ["h","he","hel","hell","hello","e","el","ell","ello","l","ll","llo","l","lo","o",""])
  , ((genSubstrings "goodbye") `setEq` ["g","go","goo","good","goodb","goodby","goodbye","o","oo","ood","oodb","oodby","oodbye","o","od","odb","odby","odbye","d","db","dby","dbye","b","by","bye","y","ye","e",""])
  , ((genSubstrings "haskell") `setEq` ["h","ha","has","hask","haske","haskel","haskell","a","as","ask","aske","askel","askell","s","sk","ske","skel","skell","k","ke","kel","kell","e","el","ell","l","ll","l",""])
  , ((replacePrefix ("","fun") "ctional") == "functional")
  , ((replacePrefix ("be","spe") "bear") == "spear")
  , ((replacePrefix ("hel","jel") "hello") == "jello")
  , ((replacePrefix ("know","") "knowledge") == "ledge")
  , ((replacePrefix ("bel","pil") "bellow") == "pillow")
  , ((replaceString ("be","spe") "") == "")
  , ((replaceString ("hel","jel") "hello") == "jello")
  , ((replaceString ("ledge","ing") "knowledge") == "knowing")
  , ((replaceString ("crossing","castle") "crossing guard") == "castle guard")
  , ((replaceString ("oon","rowni") "boone") == "brownie")
  , ((lookUp 'c' [('x','c'), ('c','z')]) == 'z')
  , ((lookUp 'c' [('a','z'), ('c','x'), ('b','y')]) == 'x')
  , ((lookUp 'c' [('a','z'), ('b','y'), ('c','x')]) == 'x')
  , ((lookUp 'b' [('a','z'), ('b','y'), ('c','x')]) == 'y')
  , ((lookUp 'z' [('a','z'), ('z','b')]) == 'b')
  , ((encode [(x,x) | x <- ['a'..'z']] "hello") == "hello")
  , ((encode [('a','z'),('i','k'),('j','j'),('k','i')] "ijk") == "kji")
  , ((encode [('a','1'), ('b','2'), ('c','3')] "abc") == "123")
  , ((encode [('e','i'), ('l','p'), ('h','q'),('o','a')] "hello") == "qippa")
  , ((encode [('b','y'), ('c','z'), ('a','z')] "abc") == "zyz")
  , ((makeTable "" "xyzw") == [])
  , ((makeTable "abcd" "") == [])
  , ((makeTable "abcd" "xyzw") == [('a','x'),('b','y'),('c','z'),('d','w')])
  , ((makeTable "abc" "xyzw") == [('a','x'),('b','y'),('c','z')])
  , ((makeTable "abcd" "xyz") == [('a','x'),('b','y'),('c','z')])
  ]

subset :: (Eq a) => [a] -> [a] -> Bool
subset = flip (all . (flip elem))

setEq :: (Eq a) => [a] -> [a] -> Bool
setEq l1 l2 = subset l1 l2 && subset l2 l1

main = putStrLn $ show (length (filter id tests)) ++ '/' : show (length tests)