import Data.List

poly2 ::Double->Double->Double->Double -> Double
poly2 a b c x = a*x^2+b*x+c

eeny::Integer->String
eeny x= if(x `mod` 2 ==0)
    then "eeny"
    else "meeny"
    

fizzbuzz:: Integer->String
fizzbuzz x 
    | x `mod` 3 == 0 && x `mod` 5 == 0 = "fizzbuzz"
    | x `mod` 3 == 0 = "fizz"
    | x `mod` 5 == 0 = "buzz"
    |otherwise=""


tribonacci :: Integer->Integer
tribonacci n
    |n==1 =1
    |n==2 =1
    |n==3 =2
    |otherwise = tribonacci(n-1)+tribonacci(n-2)+tribonacci(n-3)


binomial:: Integer->Integer->Integer
binomial n k
    |k==0 =1
    |n==0 =1
    |otherwise = binomial(n-1) k + binomial(n-1) (k-1)


verifL::[Int]->Bool
verifL x 
    |length x `mod` 2 == 0 = True
    |otherwise = False

takefinal :: [Int]->Int->[Int]
takefinal l n 
    | n > length l = l
    | otherwise =  drop (length l -n) l


remove :: [Int]->Int->[Int]
remove l n = take (n) l ++ drop (n +1 ) l

semiPareRec :: [Int] -> [Int]
semiPareRec [] = []
semiPareRec (h:t)
    | even h = h `div` 2 : t'
    | otherwise = t'
    where t' = semiPareRec t

myreplicate::Int->v->[v]
myreplicate 0 _ =[]
myreplicate n v = v:myreplicate(n-1)v
    

sumImp:: [Int]->Int
sumImp []=0
sumImp (h:t)
    |odd h = h+t'
    |otherwise=t'
    where t'=sumImp t


totalLen::[String]->Int
totalLen []=0
totalLen (h:t)
    |head(h)=='A'=length(h)+t'
    |otherwise=t'
    where t'=totalLen t
