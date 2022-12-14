import Data.Char (digitToInt, isDigit)
--1
isVowel :: Char -> Bool
isVowel x = x `elem` "aeiouAEIOU"

vocale :: String -> Int
vocale [] = 0
vocale  (h:t)
    |isVowel h == True = 1+t'
    |otherwise=t'
    where t'=vocale t

nrVocale :: [String] -> Int
nrVocale []=0
nrVocale  (h:t)
    |h == reverse h = vocale h + t'
    |otherwise = t'
    where t'=nrVocale t

--2
f :: Int -> [Int] -> [Int]
f x[] = []
f x (h:t)
    |even h =  h : x : t'
    |otherwise = h : t'
    where t' = f x t


--ex
semiPareComp :: [Int] -> [Int]
semiPareComp l = [ x `div` 2 | x <- l, even x ]

--3
divizori :: Int -> [Int]
divizori n = [x| x <- [1..n],n `mod` x == 0]

--4
listadiv::[Int] -> [[Int]]
listadiv l = [ divizori nr|nr <- l]

--5a
inIntervalRec :: Int -> Int-> [Int] -> [Int]
inIntervalRec _ _ [] = []
inIntervalRec a b (h:t)
    |elem h [a..b] == True = h:t'
    |otherwise = t'
    where t'=inIntervalRec a b t

--5b
inIntervalComp a b list = [el|el<-list, elem el [a..b]]

--6a
pozitiveRec :: [Int] -> Int
pozitiveRec [] = 0
pozitiveRec (h:t)
    |h>0 = 1+t'
    |otherwise=t'
    where t'=pozitiveRec t

--6b
pozitiveComp :: [Int] -> Int
pozitiveComp list =length [x|x<-list,x<0] 

--7a

pozitiiImpareRec [] v = []
pozitiiImpareRec (x:xs) v
    |odd x = v : pozitiiImpareRec xs (v+1)
    |otherwise = pozitiiImpareRec xs (v+1)

pozi :: [Int]->[Int]
pozi x = pozitiiImpareRec x 0

--7b
pozitiiImpareConf :: [Int] -> [Int]
pozitiiImpareConf x = [y|(z,y)<-zip x [0..],odd z] 

--8b

multDigitsRec :: String -> Int
multDigitsRec "" = 1
multDigitsRec (h:t)
    |isDigit h = digitToInt h * multDigitsRec t
    |otherwise = multDigitsRec t

multDigitsComp :: String -> Int
multDigitsComp s = product [digitToInt e |e <-s, isDigit e]
