import Distribution.Simple.Setup (falseArg)
data Fruct
  = Mar String Bool
  | Portocala String Int

ionatanFaraVierme = Mar "Ionatan" False
goldenCuVierme = Mar "Golden Delicious" True
portocalaSicilia10 = Portocala "Sanguinello" 10
listaFructe = [Mar "Ionatan" False,
                Portocala "Sanguinello" 10,
                Portocala "Valencia" 22,
                Mar "Golden Delicious" True,
                Portocala "Sanguinello" 15,
                Portocala "Moro" 12,
                Portocala "Tarocco" 3,
                Portocala "Moro" 12,
                Portocala "Valencia" 2,
                Mar "Golden Delicious" False,
                Mar "Golden" False,
                Mar "Golden" True]

--1
--a
ePortocalaDeSicilia :: Fruct -> Bool
ePortocalaDeSicilia (Mar _ _) = False 
ePortocalaDeSicilia ( Portocala s _ ) = if( s == "Tarocco" || s == "Moro" || s == "Sanguinello" ) then True
                        else False


-- test_ePortocalaDeSicilia1 =
--     ePortocalaDeSicilia (Portocala "Moro" 12) == True
-- test_ePortocalaDeSicilia2 =
--     ePortocalaDeSicilia (Mar "Ionatan" True) == False


--b
-- nrFeliiP::Fruct -> Int
-- nrFeliiP (Portocala soi nr) =nr
-- nrFeliiSicilia :: [Fruct] -> Int
-- nrFeliiSicilia []= 0
-- nrFeliiSicilia (h:t)
--     |ePortocalaDeSicilia(h)==True==nrFeliiP(h) +nrFeliiSicilia t
--     |otherwise = nrFeliiSicilia t

--var2
nrFeliiSicilia :: [Fruct] -> Int
nrFeliiSicilia l= sum [felii |Portocala soi felii <- l, ePortocalaDeSicilia(Portocala soi felii)]



-- test_nrFeliiSicilia = nrFeliiSicilia listaFructe == 52

--c
nrMereViermi :: [Fruct] -> Int
nrMereViermi l = sum [ 1 | Mar soi viermi <-l , viermi==True]

-- test_nrMereViermi = nrMereViermi listaFructe == 2

--2
type NumeA = String
type Rasa = String
data Animal = Pisica NumeA | Caine NumeA Rasa
    deriving Show

--a
vorbeste :: Animal -> String
vorbeste (Pisica _ )  = "meow"
vorbeste (Caine _ _)  = "woof"

--b
rasa :: Animal -> Maybe String
rasa (Caine _ r) = Just r
rasa (Pisica _) = Nothing


--3
data Linie = L [Int]
   deriving Show
data Matrice = M [Linie]
   deriving Show

--a
verifica :: Matrice -> Int -> Bool
verifica ( M l ) nr = foldr (&&) True [sum x == nr | L x<-l] 


-- test_verif1 = verifica (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 10 == False
-- test_verif2 = verifica (M[L[2,20,3], L[4,21], L[2,3,6,8,6], L[8,5,3,9]]) 25 == True

--b
doarPozN :: Matrice -> Int -> Bool
doarPozN  (M linii) n = foldr (&&) True [all (>0) l | L l <- linii, length l == n]

-- testPoz1 = doarPozN (M [L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == True
-- testPoz2 = doarPozN (M [L[1,2,-3], L[4,5], L[2,3,6,8], L[8,5,3]]) 3 == False

--c

corect :: Matrice -> Bool
corect (M []) = True
corect (M [x]) = True
corect (M (L x:L y:t))
    |length x == length y = corect (M (L y:t))
    |otherwise = False


-- testcorect1 = corect (M[L[1,2,3], L[4,5], L[2,3,6,8], L[8,5,3]]) == False
-- testcorect2 = corect (M[L[1,2,3], L[4,5,8], L[3,6,8], L[8,5,3]]) == True