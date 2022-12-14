import System.Win32 (COORD(xPos))
--1
sumImp:: [Int] -> Int
sumImp [] = 0
sumImp ls =sum(map(\x -> x*x)(filter odd ls))

--2
foldTrue :: [Bool] -> Bool
foldTrue xs = foldr (&&)True xs


--3
allVerifies :: (Int -> Bool) -> [Int] -> Bool
allVerifies p xs =  foldTrue (map p xs)  

--4
foldAny:: [Bool] -> Bool
foldAny xs = foldr (||)False xs
anyVerifies :: (Int -> Bool) -> [Int] -> Bool
anyVerifies p xs =  foldAny (map p xs)

--5
mapFoldr :: (a->b)->[a]->[b]
mapFoldr f xs = foldr (\x ys -> (f x):ys) [] xs

mapFilter::(a -> Bool) -> [a] -> [a]
mapFilter f = foldr(\x xs -> if f x then x:xs else xs)[]

--6
listToInt :: [Int] -> Int
listToInt = foldl (\x y -> 10*x + y) 0  

-- 7.
--a
rmChar :: Char -> String -> String
rmChar c s = filter(/=c) s

--b
rmCharsRec :: String -> String -> String
rmCharsRec "" s = s
rmCharsRec (h:t) s = rmCharsRec t (rmChar h s)

--c
rmCharsFold :: String -> String -> String
rmCharsFold l s = foldr rmChar s l