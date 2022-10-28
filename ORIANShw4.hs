
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

--2 done
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


--testing
tests =
  [ ((mapPair mod [(5,5),(4,2),(10,3)]) == [0,0,1])
  , ((mapPair div [(1,1),(2,1),(4,2),(4,20),(5,25)]) == [1,2,2,0,0])
  , ((mapPair (\x y -> even (div x y)) [(1,1),(1,2),(2,4),(20,4),(20,5)]) == [False,True,True,False,True])
  , ((mapPair (\s n -> length s > n) [("hello",2),("world",6)]) == [True,False])
  , ((mapPair (\c s -> c == head s) [('a',"hello"),('w',"world"),('b',"abacus")]) == [False,True,False])
  , ((mapPair' mod [(5,5),(2,4),(3,10)]) == [0,0,1])
  , ((mapPair' div [(1,1),(2,1),(4,2),(4,20),(5,25)]) == [1,0,0,5,5])
  , ((mapPair' (\x y -> even (div x y)) [(1,1),(2,1),(4,2),(4,20),(5,25)]) == [False,True,True,False,False])
  , ((mapPair' (\s n -> length s > n) [(2,"hello"),(6,"world")]) == [True,False])
  , ((mapPair' (\c s -> c == head s) [("hello",'a'),("world",'w'),("abacus",'b')]) == [False,True,False])
  , ((diff [10,20,30] [5,6,7]) == [5,14,23])
  , ((diff [5,6,7] [1,11,111]) == [4,-5,-104])
  , ((diff [10,20..50] [1,1..]) == [9,19,29,39,49])
  , ((diff [5,10..50] [1..5]) == [4,8,12,16,20])
  , ((diff [7,14..49] [3,6..]) == [4,8,12,16,20,24,28])
  , ((splice ["blue","green","high"] [" sky is ", " grass ", "-school-"]) == ["blue sky is blue","green grass green","high-school-high"])
  , ((splice ["left","bench","give"] [" right "," press "," take "]) == ["left right left","bench press bench","give take give"])
  , ((splice ["lather","beach","up"] ["-rinse-"," bum "," down "]) == ["lather-rinse-lather","beach bum beach","up down up"])
  , ((splice ["A","alpha","O"] ["BB"," beta ","_"]) == ["ABBA","alpha beta alpha","O_O"])
  , ((splice ["eight","super","seven"] ["-two-"," duper ","-forty-"]) == ["eight-two-eight","super duper super","seven-forty-seven"])
  , ((sqLens []) == [])
  , ((sqLens [""]) == [0])
  , ((sqLens ["hello"," ","world"]) == [25,1,25])
  , ((sqLens ["one","two","three"]) == [9,9,25])
  , ((sqLens ["one","hundred","percent"]) == [9,49,49])
  , ((bang []) == [])
  , ((bang [""]) == ["!"])
  , ((bang ["Hello","World"]) == ["Hello!","World!"])
  , ((bang ["one","two","three","four"]) == ["one!","two!","three!","four!"])
  , ((bang ["alpha","beta","gamma"]) == ["alpha!","beta!","gamma!"])
  , ((digitsOnly [123]) == [])
  , ((digitsOnly [1..10]) == [1,2,3,4,5,6,7,8,9])
  , ((digitsOnly [0,2..20]) == [0,2,4,6,8])
  , ((digitsOnly [-3,-2,-1,0,1,2,3]) == [0,1,2,3])
  , ((digitsOnly [5,6,-1,10,-3,50,7]) == [5,6,7])
  , ((removeXs ["remove","Xs"]) == ["remove"])
  , ((removeXs ["remove"," Xs"]) == ["remove"," Xs"])
  , ((removeXs ["ABC","XYZ","32","","X32"]) == ["ABC","32",""])
  , ((removeXs ["Xabc","def","Xavier","jam"]) == ["def","jam"])
  , ((removeXs ["ZYX","XWV","CBA"]) == ["ZYX","CBA"])
  , ((findNum 0 [1..5]) == False)
  , ((findNum 5 [1..5]) == True)
  , ((findNum 4 [1..5]) == True)
  , ((findNum 49 [0,5..50]) == False)
  , ((findNum 27 [0,3..39]) == True)
  , ((findNum' 0 [1..5]) == False)
  , ((findNum' 5 [1..5]) == True)
  , ((findNum' 4 [1..5]) == True)
  , ((findNum' 49 [0,5..50]) == False)
  , ((findNum' 27 [0,3..39]) == True)
  , ((exists even [1,3..9]) == False)
  , ((exists even [1,2,5,7,9]) == True)
  , ((exists (\x -> x == 'e') "Hello") == True)
  , ((exists (\x -> x == 'z') "Hello") == False)
  , ((exists (\(x,_) -> x == 3) [(1,2),(2,3),(3,4)]) == True)
  , ((exists' even [1,3..9]) == False)
  , ((exists' even [1,2,5,7,9]) == True)
  , ((exists' (\x -> x == 'e') "Hello") == True)
  , ((exists' (\x -> x == 'z') "Hello") == False)
  , ((exists' (\(x,_) -> x == 3) [(1,2),(2,3),(3,4)]) == True)
  , ((noDups [1,2,2,3,3,3]) `permEq` [3,2,1])
  , ((noDups [4,5,1,9,1,0]) `permEq` [0,1,9,5,4])
  , ((noDups [(1,2),(2,1),(3,4),(2,1)]) `permEq` [(2,1),(3,4),(1,2)])
  , ((noDups "a b c d e f") `permEq` "f edcba")
  , ((noDups ["hello","help","world","hello","quirk"]) `permEq` ["quirk","hello","world","help"])
  , ((noDups' [1,2,2,3,3,3]) `permEq` [1,2,3])
  , ((noDups' [4,5,1,9,1,0]) `permEq` [4,5,1,9,0])
  , ((noDups' [(1,2),(2,1),(3,4),(2,1)]) `permEq` [(1,2),(2,1),(3,4)])
  , ((noDups' "a b c d e f") `permEq` "a bcdef")
  , ((noDups' ["hello","help","world","hello","quirk"]) `permEq` ["hello","help","world","quirk"])
  , ((countOverflow 5 []) == 0)
  , ((countOverflow 3 ["hello","abc","world","true"]) == 3)
  , ((countOverflow 5 ["abc","123","vwxyz"]) == 0)
  , ((countOverflow 0 ["hello","world",""," "]) == 3)
  , ((countOverflow 4 ["Hello","again","this","and","that"]) == 2)
  , ((countOverflow' 5 []) == 0)
  , ((countOverflow' 3 ["hello","abc","world","true"]) == 3)
  , ((countOverflow' 5 ["abc","123","vwxyz"]) == 0)
  , ((countOverflow' 0 ["hello","world",""," "]) == 3)
  , ((countOverflow' 4 ["Hello","again","this","and","that"]) == 2)
  , ((concatList [[1,2,3],[4],[5,6]]) == [1,2,3,4,5,6])
  , ((concatList [[1,2],[3],[3,2,1]]) == [1,2,3,3,2,1])
  , ((concatList ["hello"," ","world"]) == "hello world")
  , ((concatList ["blue"," ","sky is"," ","blue"]) == "blue sky is blue")
  , ((concatList [[(1,1),(2,2)],[(3,3),(4,4)],[(5,5)]]) == [(1,1),(2,2),(3,3),(4,4),(5,5)])
  , ((concatList' [[1,2,3],[4],[5,6]]) == [1,2,3,4,5,6])
  , ((concatList' [[1,2],[3],[3,2,1]]) == [1,2,3,3,2,1])
  , ((concatList' ["List","Of","Strings"]) == "ListOfStrings")
  , ((concatList' ["blue"," ","sky is"," ","blue"]) == "blue sky is blue")
  , ((concatList' [[(1,1),(2,2)],[(3,3),(4,4)],[(5,5)]]) == [(1,1),(2,2),(3,3),(4,4),(5,5)])
  , ((bindList show [123,456,789]) == "123456789")
  , ((bindList (\x -> [x,x]) [1..5]) == [1,1,2,2,3,3,4,4,5,5])
  , ((bindList (\(x,y) -> [(x - y),(y - x)]) [(5,1),(4,2),(3,3)]) == [4,-4,2,-2,0,0])
  , ((bindList (\s -> [x | x <- s, elem x "aeiou"]) ["hello","again","world"]) == "eoaaio")
  , ((bindList (\x -> [y | y <- [1..x]]) [1..5]) == [1,1,2,1,2,3,1,2,3,4,1,2,3,4,5])
  , ((bindList' show [123,456,789]) == "123456789")
  , ((bindList' (\x -> [x,x]) [1..5]) == [1,1,2,2,3,3,4,4,5,5])
  , ((bindList' (\(x,y) -> [(x - y),(y - x)]) [(5,1),(4,2),(3,3)]) == [4,-4,2,-2,0,0])
  , ((bindList' (\s -> [x | x <- s, elem x "aeiou"]) ["hello","again","world"]) == "eoaaio")
  , ((bindList' (\x -> [y | y <- [1..x]]) [1..5]) == [1,1,2,1,2,3,1,2,3,4,1,2,3,4,5])
  ]

subset :: (Eq a) => [a] -> [a] -> Bool
subset = flip (all . (flip elem))

setEq :: (Eq a) => [a] -> [a] -> Bool
setEq l1 l2 = subset l1 l2 && subset l2 l1

permEq :: (Eq a) => [a] -> [a] -> Bool
permEq l1 l2 = setEq l1 l2 && length l1 == length l2

main = putStrLn $ show (length (filter id tests)) ++ '/' : show (length tests)
getErrors = map fst . filter (not . snd) . zip [1..] $ tests