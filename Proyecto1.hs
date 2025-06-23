{- CONSTANZA FERNÁNDEZ BODEREAU, PROYECTO 1 -}

---------------------------1a) ES CERO------------------------------------

esCero :: Int -> Bool
esCero x = x == 0

-- *Main> esCero 1
-- False
-- *Main> esCero 0
-- True

---------------------------1b) ES POSITIVO--------------------------------

esPositivo :: Int -> Bool
esPositivo x = x > 0

{- *Main> esPositivo (-2)
False
*Main> esPositivo 34234234234
True -}

----------------------------1c) ES VOCAL--------------------------------

esVocal :: Char -> Bool
esVocal x = x == 'a' || x == 'e' || x == 'i' || x == 'o' || x == 'u'

{- *Main> esVocal 'a'
True
*Main> esVocal 'b'
False -}

----------------------------1d) VALOR ABSOLUTO---------------------------

valorAbsoluto :: Int -> Int
valorAbsoluto x | x >= 0 = x
                | x < 0 = x * (-1)
                
{- *Main> valorAbsoluto 2
2
*Main> valorAbsoluto (-123)
123 -}

----------------------------2a) PARATODO--------------------------------

paratodo :: [(Bool)] -> Bool
paratodo [] = True
paratodo (x:xs) = (x == True) && paratodo xs 

{-*Main> paratodo [False, True]
False
*Main> paratodo [True, True]
True -}

---------------------------2b) SUMATORIA--------------------------------

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

{- *Main> sumatoria [10,234,23423]
23667
*Main> sumatoria [1,2,3]
6 -}

----------------------------2c) PRODUCTORIA----------------------------

productoria :: [Int] -> Int
productoria [] = 0
productoria (x:xs) = x * sumatoria xs

{- *Main> productoria [1,2,3,4]
9
*Main> productoria [0,3,4]
0 -}

----------------------------2d) FACTORIAL--------------------------------

factorial :: Int -> Int
factorial 0 = 1
factorial x = x*factorial (x-1)

{- *Main> factorial 3
6
*Main> factorial 0
1 -}

------------------------------2e) PROMEDIO------------------------------

promedio :: [Int] -> Int
promedio xs = div (sumatoria xs) (length xs)  

{- *Main> promedio [6,7,8,9]
7
*Main> promedio [0,0,2,3]
1 -}

------------------------------- 3 PERTENECE------------------------------

pertenece :: Int -> [Int] -> Bool
pertenece n [] = False
pertenece n (x:xs) = (n == x) || pertenece n xs

{- *Main> pertenece 5 [1,2,3,4]
False
*Main> pertenece 15 [21,12,3,15,0,4]
True -} 

---------------------------------- 4a) PARATODO'-------------------------

paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] t = True
paratodo' (x:xs) t = t x && paratodo' xs t

-- *Main> paratodo' [1,2,3] esPositivo 
-- True
-- *Main> paratodo' [1,-2,3] esPositivo 
-- False

----------------------------4b) EXISTE'-----------------------------------

existe' :: [a] -> (a -> Bool) -> Bool
existe' [] t = False
existe' (x:xs) t = t x || existe' xs t

-- *Main> existe' [1,9,0] esCero 
-- True
-- *Main> existe' [] esVocal 
-- False

----------------------------4c) SUMATORIA'---------------------------------

sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] t = 0
sumatoria' (x:xs) t = (t x )+(sumatoria' xs t)

-- *Main> sumatoria' [1,3,4] factorial 
-- 31
-- *Main> sumatoria' [1,-3,4] valorAbsoluto 
-- 8

----------------------------4d) PRODUCTORIA'-------------------------------

productoria' :: [a] -> (a -> Int) -> Int
productoria' [] t = 1
productoria' (x:xs) t = (t x) * (productoria' xs t)

-- *Main> productoria' [1,2,3] factorial
-- 12
-- *Main> productoria' [-2,5,7] valorAbsoluto 
-- 70

--------------------------- 5) PARATODO -----------------------------------

paratodo3 :: [a] -> (a -> Bool) -> Bool
paratodo3 xs t =  paratodo' xs t

-- *Main> paratodo3 [0,0,0] esCero 
-- True
-- *Main> paratodo3 "haskell" esVocal 
-- False

----------------------------6a) TODOSPARES----------------------------------
todosPares :: [Int] -> Bool
todosPares xs = paratodo' xs even

-- *Main> todosPares [2,4,6]
-- True
-- *Main> todosPares [2,4,3]
-- False

----------------------------6b) HAYMULTIPLO--------------------------------

esMultiplo :: Int -> Int -> Bool
esMultiplo t n = (mod n t == 0)

hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo t xs = existe' xs (esMultiplo t)

-- *Main> hayMultiplo 3 [2,4,8]
-- False
-- *Main> hayMultiplo 3 [303,30,2]
-- True

----------------------------6c) SUMACUADRADOS-------------------------------

darCuadrado :: Int -> Int
darCuadrado n = n*n

sumaCuadrados :: Int -> Int
sumaCuadrados n = sumatoria' [0.. (n-1)] darCuadrado

-- *Main> sumaCuadrados 23
--3795
-- *Main> sumaCuadrados 5
--30

----------------------------6d) EXISTEDIVISOR-------------------------------

esDivisor :: Int -> Int -> Bool
esDivisor n x = mod n x == 0

existeDivisor :: Int-> [Int] -> Bool
existeDivisor n ls = existe' ls (esDivisor n)

-- *Main> existeDivisor 2 [2,3,4,56]
-- True
-- *Main> existeDivisor 11 [2,3,4]
-- False

----------------------------6e) ESPRIMO-------------------------------------

esPrimo :: Int -> Bool
esPrimo x = not (existeDivisor x [2..(x-1)])

-- *Main> esPrimo 11
-- True
-- *Main> esPrimo 10
-- False

----------------------------6f) FACTORIAL REDEFINIR-------------------------

factorial2 :: Int -> Int
factorial2 n = factorial n

-- *Main> factorial2 1
-- 1
-- *Main> factorial2 5
-- 120

----------------------------6g) MULTIPLICAPRIMOS----------------------------

listaPrimos:: [Int] -> [Int]
listaPrimos [] = []
listaPrimos (x:xs) | esPrimo x == True = x : listaPrimos xs
                   | otherwise = listaPrimos xs

multiplicaPrimos :: [Int] -> Int
multiplicaPrimos xs = productoria (listaPrimos xs)

{- *Main> multiplicaPrimos [2,3,4,6]
6
*Main> multiplicaPrimos [11,2,0]
22 -}

----------------------------6h) ESFIB----------------------------------------

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

esFib :: Int -> Bool
esFib n = existe' (map fib [0..n]) (==n)

-- *Main> esFib 8
-- True
-- *Main> esFib 9
-- False

----------------------------6i) TODOSFIB--------------------------------

todosFib :: [Int] -> Bool
todosFib xs = paratodo' xs esFib

-- *Main> todosFib [1,2,3,4]
-- False
-- *Main> todosFib [1,1,8,13]
-- True

---------------------------- 7) ----------------------------------------

{- ¿Que hacen estas funciones? 
MAP toma un predicado y una lista. Este, le aplica una operación a cada elemento de la lista.
FILTER toma una lista y devuelve los elementos de xs que cumplen con alguna propiedad.

¿A que equivale la expresion map succ [1, -4, 6, 2, -8], donde succ n = n+1?
MAP suma 1 a cada elemento de la lista, creando así: [2,-3,7,3,-7]

¿Y la expresion filter esPositivo [1, -4, 6, 2, -8]? 
FILTER va a tomar los números positivos de mi lista, y creará una nueva lista sólo con esos números: [1,6,2] -}

---------------------------- 8) ----------------------------------------

duplicado :: [Int] -> [Int]
duplicado [] = []
duplicado (x:xs) = (x * 2) : duplicado xs

duplicadoMap :: [Int] -> [Int]
duplicadoMap xs = map (*2) xs

-- *Main> duplicadoMap [1,2,3]
-- [2,4,6]
-- *Main> duplicadoMap [99,12,0]
-- [198,24,0]

---------------------------- 9)----------------------------------------

{- listaPrimos :: [Int] -> [Int]
listaPrimos [] = []
listaPrimos (x:xs) | esPrimo x == True = x : listaPrimos xs
                   | otherwise = listaPrimos xs 
                   YA DEFINIDA ANTERIORMENTE -}
                   
listaPrimosFilter  :: [Int] -> [Int]
listaPrimosFilter xs = filter esPrimo xs

-- *Main> listaPrimosFilter [1,2,3,4,5,6,7,8,9]
-- [1,2,3,5,7]
-- *Main> listaPrimosFilter [0,0,2,3]
-- [0,0,2,3]

-------------------------- 10) ----------------------------------------

primIgualesA1 :: Eq a => a -> [a] -> [a]
primIgualesA1 n [] = []
primIgualesA1 n (x:xs) | n == x = n : primIgualesA1 n xs
                      | n /= x = []

{- *Main> primIgualesA1 1 [1,2,1,3]
[1]
*Main> primIgualesA1 0 [0,0,0,2,3,0]
[0,0,0] -}

primIgualesA2  :: Eq a => a -> [a] -> [a]
primIgualesA2 n xs = takeWhile (==n) xs

-- *Main> primIgualesA2 2 [1,2,2,3]
-- []
-- *Main> primIgualesA2 2 [2,2,2,3]
-- [2,2,2]

-------------------------- 11) ---------------------------------------

primIguales3 :: Eq a => [a] -> [a]
primIguales3 [] = []
primIguales3 (x:y:xs) | x == y = x : primIguales3 (y:xs)
                      | otherwise = [x]

-- *Main> primIguales3 [3,3,4,3]
-- [3,3]
-- *Main> primIguales3 [1,2,3,4,0]
-- [1]

primIguales4 :: Eq a => [a] -> [a]
primIguales4 [] = []
primIguales4 (x:xs) = x: primIgualesA1 x xs 

{- *Main> primIguales4 []
[]
*Main> primIguales4 [1,0,0,2,1,4]
[1] -} 

