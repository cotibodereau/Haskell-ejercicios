-- CONSTANZA FERNÁNDEZ BODEREAU, PROYECTO 2 --

--------------------------- 1A) ---------------------------------

data Carrera = Matematica | Fisica | Computacion | Astronomia deriving Eq

--------------------------- 1B) ---------------------------------

titulo :: Carrera -> String
titulo Matematica = "Licenciatura en Matematica"
titulo Fisica = " Licenciatura en Fisica"
titulo Computacion = " Licenciatura en Ciencias de la Computacion"
titulo Astronomia = "Licenciatura en Astronomia"

{- *Main> titulo Matematica 
"Licenciatura en Matematica"
*Main> titulo Computacion 
" Licenciatura en Ciencias de la Computacion" -}

--------------------------- 1C) ---------------------------------

data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si 
     deriving (Eq, Ord, Bounded, Show)

--------------------------- 1D) ---------------------------------

cifradoAmericano :: NotaBasica -> Char
cifradoAmericano Do = 'C'
cifradoAmericano Re = 'D'
cifradoAmericano Mi = 'E'
cifradoAmericano Fa = 'F'
cifradoAmericano Sol = 'G'
cifradoAmericano La = 'A'
cifradoAmericano Si = 'B'

{- *Main> cifradoAmericano Do
'C'
*Main> cifradoAmericano Sol
'G' -}

--------------------------- 2A) ---------------------------------

{- *Main> Fa `min` Sol
Fa
*Main> Do <= Re
True -} 

--------------------------- 3A) ---------------------------------

minimoElemento :: Ord a => [a] -> a
minimoElemento [x] = x
minimoElemento (x:xs) = min x (minimoElemento xs)

{- *Main> minimoElemento [Sol, Fa, Re]
Re
*Main> minimoElemento [2,3,1]
1 -}

--------------------------- 3B) ---------------------------------

minimoElemento' :: (Ord a , Bounded a) => [a] -> a
minimoElemento' [] = maxBound
minimoElemento' (x:xs) = min x (minimoElemento' xs)

{- *Main> minimoElemento' [Do, Re, Mi]
Do
*Main> minimoElemento' [Fa, Fa, Mi]
Mi -}

--------------------------- 3C) ---------------------------------

notaMasGrave :: [NotaBasica] -> NotaBasica
notaMasGrave xs = minimoElemento xs

{-*Main> notaMasGrave [Fa, La, Sol, Re, Fa]
Re -}

--------------------------- 4A) ---------------------------------

type Altura = Int
type NumCamiseta = Int
data Zona = Arco | Defensa | Mediocampo | Delantera deriving (Show, Eq)
data TipoReves = DosManos | UnaMano deriving (Show, Eq)
data Modalidad = Carretera | Pista | Monte | BMX deriving (Show, Eq)
data PiernaHabil = Izquierda | Derecha deriving (Show, Eq)
type ManoHabil = PiernaHabil

data Deportista = Ajedrecista 
                | Ciclista Modalidad 
                | Velocista Altura 
                | Tenista TipoReves ManoHabil Altura 
                | Futbolista Zona NumCamiseta PiernaHabil Altura 
                deriving (Show, Eq)

--------------------------- 4B) ---------------------------------

{- Ciclista :: Modalidad -> Deportista -}

--------------------------- 4C) ---------------------------------

contar_velocistas :: [Deportista] -> Int
contar_velocistas [] = 0
contar_velocistas ((Velocista a ) :xs) = 1 + contar_velocistas xs
contar_velocistas (x:xs) =  contar_velocistas xs 

{- *Main> contar_velocistas [ Ciclista Pista, Ajedrecista]
0
*Main> contar_velocistas [ Velocista 9, Ajedrecista , Velocista 12]
2 -}

--------------------------- 4D) ---------------------------------

contar_futbolistas :: [Deportista] -> Zona -> Int
contar_futbolistas [] z = 0
contar_futbolistas ((Futbolista Arco _ _ _) :xs) Arco = 1 + contar_futbolistas xs Arco
contar_futbolistas ((Futbolista Defensa _ _ _) :xs) Defensa = 1 + contar_futbolistas xs Defensa
contar_futbolistas ((Futbolista Mediocampo _ _ _) :xs) Mediocampo = 1 + contar_futbolistas xs Mediocampo
contar_futbolistas ((Futbolista Delantera _ _ _) :xs) Delantera = 1 + contar_futbolistas xs Delantera
contar_futbolistas (x:xs) z = contar_futbolistas xs z

{- *Main> contar_futbolistas [Futbolista Arco 9 Derecha 23, Futbolista Defensa 3 Derecha 121, Ajedrecista ] Arco
1
*Main> contar_futbolistas [Futbolista Arco 9 Derecha 23, Futbolista Defensa 3 Derecha 121, Futbolista Arco 23 Derecha 21] Arco
2 -}

--------------------------- 4E) ---------------------------------

esFutbolistaZ :: Zona -> Deportista -> Bool
esFutbolistaZ z (Futbolista x _ _ _) = x==z
esFutbolistaZ z _ = False

contar_futbolistas2 :: [Deportista] -> Zona -> Int
contar_futbolistas2 xs z = length (filter (esFutbolistaZ z) xs) 

{- *Main> contar_futbolistas2 [Futbolista Arco 9 Derecha 23, Futbolista Defensa 3 Derecha 121, Futbolista Arco 23 Derecha 21] D
Defensa     Delantera   Deportista  Derecha     Do          DosManos    Double
*Main> contar_futbolistas2 [Futbolista Arco 9 Derecha 23, Futbolista Defensa 3 Derecha 121, Futbolista Arco 23 Derecha 21] Defensa 
1 -}

--------------------------- 5A) ---------------------------------

sonidoNatural :: NotaBasica -> Int
sonidoNatural Do = 0
sonidoNatural Re = 2
sonidoNatural Mi = 4
sonidoNatural Fa = 5
sonidoNatural Sol = 7
sonidoNatural La = 9
sonidoNatural Si = 11

{- *Main> sonidoNatural Re
2
*Main> sonidoNatural Sol
7 -}

--------------------------- 5B) ---------------------------------

data Alteracion = Bemol | Natural | Sostenido

--------------------------- 5C) ---------------------------------

data NotaMusical = Nota NotaBasica Alteracion


--------------------------- 5D) ---------------------------------

alteracion :: NotaBasica -> Alteracion -> Int
alteracion x Bemol = (sonidoNatural x - 1)
alteracion x Natural = sonidoNatural x
alteracion x Sostenido = (sonidoNatural x + 1)

sonidoCromatico :: NotaMusical -> Int
sonidoCromatico  (Nota x b) = (alteracion x b)

{- OTRA SOLUCION SERIA:
sonidoCromatico :: NotaMusical -> Int
sonidoCromatico (Nota n Sostenido) = 1 + (sonidoNatural n)
sonidoCromatico (Nota n Bemol) = (sonidoNatural n) - 1
sonidoCromatico (Nota n Natural) = sonidoNatural n -}

{- *Main> alteracion Sol Bemol 
6
*Main> alteracion Sol Sostenido
8
*Main> alteracion Do Natural
0 -}

{- *Main> sonidoCromatico (Nota Do Bemol)
-1
*Main> sonidoCromatico (Nota Re Sostenido)
3 -}

--------------------------- 5E) ---------------------------------

instance Eq NotaMusical where
         nota1 == nota2 = sonidoCromatico nota1 == sonidoCromatico nota2

{- *Main> Nota Do Sostenido == Nota Re Bemol
True 
-}

--------------------------- 5F) ---------------------------------

instance Ord NotaMusical where
         nota1 <= nota2 = sonidoCromatico nota1 <= sonidoCromatico nota2

{-*Main> Nota Do Sostenido <= Nota Sol Natural 
True 
-}

--------------------------- 6) ----------------------------------

primerElemento :: [a] -> Maybe a
primerElemento [] = Nothing
primerElemento (x:xs) = (Just x)

{- *Main> primerElemento [3,2,34,2]
Just 3
*Main> primerElemento []
Nothing -}

--------------------------- 7,1) --------------------------------

data Cola = VaciaC | Encolada Deportista Cola deriving (Show, Eq)

atender :: Cola -> Maybe Cola
atender VaciaC = Nothing
atender (Encolada _ VaciaC) = Just VaciaC
atender (Encolada _ (Encolada a b)) = Just (Encolada a b)

{- *Main> atender VaciaC 
Nothing
*Main> atender (Encolada Ajedrecista ( Encolada (Ciclista BMX) VaciaC ))
Just (Encolada (Ciclista BMX) VaciaC) -} 

--------------------------- 7,2) --------------------------------

encolar :: Deportista -> Cola -> Cola
encolar a VaciaC = Encolada a VaciaC
encolar a (Encolada b c) = Encolada b (encolar a c)

{- *Main> encolar Ajedrecista ( Encolada (Tenista UnaMano Derecha 9) VaciaC )
Encolada (Tenista UnaMano Derecha 9) (Encolada Ajedrecista VaciaC)
*Main> encolar (Velocista 4) VaciaC 
Encolada (Velocista 4) VaciaC -} 

--------------------------- 7,3) --------------------------------

busca :: Cola -> Zona -> Maybe Deportista
busca VaciaC z = Nothing
busca (Encolada Ajedrecista y ) z = busca y z
busca (Encolada (Ciclista _ ) y ) z = busca y z
busca (Encolada (Velocista _) y ) z = busca y z
busca (Encolada (Tenista _ _ _) y ) z = busca y z
busca (Encolada (Futbolista q w e r) y ) z | q == z = Just (Futbolista q w e r)
                                           | otherwise = busca y z


{- *Main> busca ( Encolada Ajedrecista VaciaC) Arco 
Nothing
*Main> busca (Encolada (Futbolista Arco 9 Derecha 9) VaciaC ) Delantera  
Nothing
*Main> busca (Encolada (Futbolista Arco 10 Izquierda 14) VaciaC ) Arco 
Just (Futbolista Arco 10 Izquierda 14) -}

--------------------------- 7B) ---------------------------------

{- ¿A qué otro tipo se parece Cola?

Cola se parece al tipo "Lista" , donde VaciaC se parece a [] -}

--------------------------- 8A) ---------------------------------

data ListaAsoc a b = Vacia | Nodo a b (ListaAsoc a b) deriving (Show)

{- Para que ListaAsoc presente la informacion de una guia telefonica debemos instanciar 'a' como un string y 'b' como un Int, donde en string pongo los nombres que deseo buscar y en Int los numeros que me va a devoler -} 

type GuiaTel = ListaAsoc String Int

--------------------------- 8,1) --------------------------------

la_long :: ListaAsoc a b -> Int
la_long Vacia = 0
la_long (Nodo _ _ a ) = 1 + la_long a

{- *Main> la_long (Nodo 3 2 (Nodo 3 2 Vacia)) 
2
*Main> la_long Vacia
0 -}

--------------------------- 8,2) --------------------------------

la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b
la_concat Vacia d = d
la_concat (Nodo a b c ) d = Nodo a b (la_concat c d) 

{- *Main> la_concat Vacia Vacia
Vacia
*Main> la_concat (Nodo 1 23 Vacia) Vacia
Nodo 1 23 Vacia -} 

--------------------------- 8,3) --------------------------------

la_agregar :: Eq a => ListaAsoc a b -> a -> b -> ListaAsoc a b
la_agregar Vacia x y = Nodo x y Vacia
la_agregar (Nodo a b la) x y | a==x = Nodo x y la
                             | a/= x = Nodo a b (la_agregar la x y)

{- *Main> la_agregar (Nodo 2 3 Vacia) 2 3
Nodo 2 3 (Nodo 2 3 Vacia)
*Main> la_agregar (Nodo 54 12 Vacia) 2 3
Nodo 2 3 (Nodo 54 12 Vacia) -} 

--------------------------- 8,4) --------------------------------

-- transforma la lista en una lista de pares-dato --

la_pares :: ListaAsoc a b -> [(a,b)]
la_pares Vacia = []
la_pares (Nodo a b c) = (a,b) : la_pares c 

{- *Main> la_pares Vacia
[]
*Main> la_pares (Nodo 3 2 Vacia)
[(3,2)]
*Main> la_pares (la_concat (Nodo 1 23 Vacia) Vacia)
[(1,23)] -}

--------------------------- 8,5) --------------------------------

la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca Vacia _ = Nothing
la_busca (Nodo a b c) d | a == d = Just b
                        | otherwise = la_busca c d

{- *Main> la_busca (Nodo 2 3 Vacia) 2
Just 3
*Main> la_busca (Nodo 2 3 Vacia) 3
Nothing -}

--------------------------- 8,6) --------------------------------

la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b
la_borrar _ Vacia = Vacia 
la_borrar d (Nodo a b c) | d == a = c
                         | otherwise = Nodo a b (la_borrar d c)

{- *Main> la_borrar 1 (Nodo 1 2 Vacia)
Vacia
*Main> la_borrar 3 (Nodo 1 2 Vacia)
Nodo 1 2 Vacia -} 

--------------------- EJERCICIOS ESTRELLA -----------------------

data Arbol a = Hoja | Rama (Arbol a) a (Arbol a) deriving (Show, Eq)

a_long :: Arbol a -> Int
a_long Hoja = 0
a_long (Rama a1 _ a3) = 1 + a_long a1 + a_long a3

{- *Main> a_long (Rama (Rama (Hoja) 3 (Hoja)) 1 Hoja)
2 -}

a_hojas :: Arbol a -> Int
a_hojas Hoja = 1
a_hojas (Rama a1 _ a3) = a_hojas a1 + a_hojas a3

{- *Main> a_hojas (Rama (Rama (Hoja) 4 (Hoja)) 4 Hoja)
3 -}

a_inc :: Num a => Arbol a -> Arbol a
a_inc Hoja = Hoja
a_inc (Rama a1 a2 a3) = Rama (a_inc a1) (a2 + 1) (a_inc a3)

{- *Main> a_inc (Rama (Rama Hoja 1 Hoja) 2 (Rama Hoja 3 Hoja))
Rama (Rama Hoja 2 Hoja) 3 (Rama Hoja 4 Hoja) -}

incrementar :: Int -> Int
incrementar x = x + 1

a_map :: (a -> b) -> Arbol a -> Arbol b
a_map _ Hoja = Hoja
a_map f (Rama a1 a2 a3) = Rama (a_map f a1) (f a2) (a_map f a3)

{- *Main> a_map incrementar (Rama (Rama (Hoja) 4 (Hoja)) 4 Hoja)
Rama (Rama Hoja 5 Hoja) 5 Hoja -}

--------------------------- 10) ---------------------------------

data ABB a = RamaABB (ABB a) a (ABB a) | VacioABB deriving (Show, Eq)

insertarABB :: Ord a => a -> ABB a -> ABB a
insertarABB x VacioABB = RamaABB VacioABB x VacioABB
insertarABB x (RamaABB a1 a2 a3) | x < a2 = RamaABB (insertarABB x a1) a2 a3
                                 | x > a2 = RamaABB a1 a2 (insertarABB x a3)
                                 | otherwise = RamaABB a1 a2 a3

{- *Main> insertarABB 3 (RamaABB VacioABB 5 VacioABB)
RamaABB (RamaABB VacioABB 3 VacioABB) 5 VacioABB

*Main> insertarABB 12 (RamaABB VacioABB 1 VacioABB)
RamaABB VacioABB 1 (RamaABB VacioABB 12 VacioABB) -}

buscarABB :: Eq a => a -> ABB a -> Bool
buscarABB _ VacioABB = False
buscarABB x (RamaABB a1 a2 a3) | (x == a2) = True
                               | otherwise = buscarABB x a1 || buscarABB x a3

{- *Main> buscarABB 4 (RamaABB (RamaABB VacioABB 10 VacioABB) 2 (RamaABB VacioABB 11 VacioABB))
False

*Main> buscarABB 3 (RamaABB (RamaABB VacioABB 3 (RamaABB VacioABB 4 VacioABB)) 5 (RamaABB VacioABB 8 VacioABB))
True -}

mayor_a_todos :: Ord a => a -> ABB a -> Bool
mayor_a_todos _ VacioABB = True
mayor_a_todos x (RamaABB a1 a2 a3) = (x > a2) && (mayor_a_todos x a1) && (mayor_a_todos x a3)
                                                            
menor_a_todos :: Ord a => a -> ABB a -> Bool
menor_a_todos _ VacioABB = True
menor_a_todos x (RamaABB a1 a2 a3) = (x < a2) && (menor_a_todos x a1) && (menor_a_todos x a3)

verificarABB :: Ord a => ABB a -> Bool
verificarABB VacioABB = True
verificarABB (RamaABB a1 a2 a3) = (menor_a_todos a2 a1) && (mayor_a_todos a2 a3) && (verificarABB a1) && (verificarABB a3)

ejemplo1 :: ABB Int
ejemplo1 = RamaABB (RamaABB VacioABB 10 VacioABB) 2 (RamaABB VacioABB 11 VacioABB)


{- *Main> verificarABB (RamaABB (RamaABB VacioABB 10 VacioABB) 2 (RamaABB VacioABB 11 VacioABB))
False 
*Main> verificarABB ejemplo1
False-}

ejemplo2 :: ABB Int
ejemplo2 = RamaABB (RamaABB VacioABB 9 VacioABB) 10 VacioABB

{- *Main> verificarABB (RamaABB (RamaABB VacioABB 9 VacioABB) 10 VacioABB)
False 
*Main> verificarABB ejemplo2
False
-}


