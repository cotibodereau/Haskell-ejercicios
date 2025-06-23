estaEnDNI :: Int -> Bool
estaEnDNI 1 = False
estaEnDNI 2 = False
estaEnDNI 8 = False
estaEnDNI _ = True


sumaDNI :: [Int] -> Int
sumaDNI [] = 0
sumaDNI (x:xs) | (estaEnDNI x == True) = x + sumaDNI xs
               | otherwise = sumaDNI xs
               
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

sumaDNI' :: [Int] -> Int
sumaDNI' = sumatoria . filter estaEnDNI

reducir :: [a] -> (a -> a -> a) -> a
reducir [x] _ = x
reducir (x:xs) f = f x (reducir xs f) 
