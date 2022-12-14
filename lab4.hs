--1
factori:: Int -> [Int]
factori n =  [x | x <- [1..n], mod n x == 0]

--2
prim :: Int -> Bool
prim n = length(factori n)==2

--3
numerePrime::Int->[Int]
numerePrime n = [x | x<-[2..n],prim x]

--4
mzip3:: [a]->[b]->[c]->[(a,b,c)]
mzip3 ls1 ls2 ls3  = [(a,b,c) | ((a,b),c)<-zip (zip ls1 ls2)ls3] 

--5
firstEl :: [(a,b)] -> [a]
firstEl  = map fst 

--6
sumList :: [[Int]] ->[Int]
sumList = map sum

--7
prel ::Int->Int
prel x  
    |odd x = x * 2
    |otherwise = div x 2
prel2 :: [Int]->[Int]
--prel2 = map prel 
prel2 = map (\ x-> if even x then x `div` 2 else 2*x)

--8 
caract:: Char -> [String] ->[String]
caract litera = filter (litera `elem`) 

--9
patrat:: [Int] -> [Int]
patrat ls = map (\x -> x*x)(filter odd ls)

--10
pozitiiImpare:: [Int] -> [Int]
pozitiiImpare ls = map(\x -> x*x) [i | (i,j)<- filter(\(i,j)->odd j)(zip ls [0..])]

--11
numaiVocale:: [String]->[String]
numaiVocale = map (filter( `elem` "aeiouAEIOU"))

--12
myMap::(a->b)->[a]->[b]
myMap f[]=[]
myMap f(x:xs)=f x :myMap f xs

myFilter::(a->Bool)->[a]->[a]
myFilter _[]=[]
myFilter f (x:xs) 
        |f x = x : myFilter f xs
        |otherwise = myFilter f xs