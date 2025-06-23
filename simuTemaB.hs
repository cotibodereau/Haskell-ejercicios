data Palo = Espada | Basto | Oro | Copa deriving (Show)


mismo_palo :: Palo -> Palo -> Bool
mismo_palo Espada Espada = True
mismo_palo Basto Basto = True
mismo_palo Oro Oro = True
mismo_palo Copa Copa = True
mismo_palo _ _ = False

{-
data Palo = Espada | Basto | Oro | Copa deriving (Show, Eq)

mismo_palo :: Palo -> Palo -> Bool
mismo_palo p1 p2 | (p1 == p2) = True
                 | otherwise = False
-}                 


data Figura = Uno | Dos | Tres | Cuatro | Cinco | Seis | Siete | Sota | Caballo | Rey deriving (Show)

valor_figura :: Figura -> Int
valor_figura Uno = 1
valor_figura Dos = 2
valor_figura Tres = 3
valor_figura Cuatro = 4
valor_figura Cinco = 5
valor_figura Seis = 6
valor_figura Siete = 7
valor_figura Sota = 8
valor_figura Caballo = 9
valor_figura Rey = 10

data Carta = Naipe Figura Palo
suma_cartas :: Carta -> Carta -> Maybe Int
suma_cartas (Naipe f1 Espada) (Naipe f2 Espada) = Just (valor_figura f1 + valor_figura f2)
suma_cartas (Naipe f1 Basto) (Naipe f2 Basto) = Just (valor_figura f1 + valor_figura f2)
suma_cartas (Naipe f1 Oro) (Naipe f2 Oro) = Just (valor_figura f1 + valor_figura f2)
suma_cartas (Naipe f1 Copa) (Naipe f2 Copa) = Just (valor_figura f1 + valor_figura f2)
suma_cartas (Naipe f1 _ ) (Naipe f2 _) = Nothing


obtenerPalo :: Carta -> Palo
obtenerPalo (Naipe _ p) = p

compatibles :: Carta -> [Carta] -> [Figura]
compatibles (Naipe f1 p1) [] = []
compatibles (Naipe f1 p1) ((Naipe f2 p2):xs) | mismo_palo p1 p2 && ((valor_figura f1 + valor_figura f2) <15) = f2 : compatibles (Naipe f1 p1) xs
                                 | otherwise = compatibles (Naipe f1 p1) xs

type Duracion = Int
type Nombre = String

data Cancion = Tema Nombre Duracion
data Estado = Escuchando | NoEscuchando
data Playlist = EnLista Cancion Estado Playlist | Vacia

duracionEscuchando :: Estado -> Cancion -> Int
duracionEscuchando Escuchando (Tema n dura) = dura
duracionEscuchando NoEscuchando (Tema n dura) = 0


tiempo_reproduccion :: Playlist -> Int
tiempo_reproduccion Vacia = 0
tiempo_reproduccion (EnLista c e p) = duracionEscuchando e c + tiempo_reproduccion p

{- *Main> tiempo_reproduccion (EnLista (Tema "Bury a friend" 3) Escuchando (EnLista (Tema "Mati se la come" 4) Escuchando Vacia))
7
*Main> tiempo_reproduccion (EnLista (Tema "Bury a friend" 3) Escuchando (EnLista (Tema "Mati se la come" 4) NoEscuchando Vacia))
3 -}

{- data Arbol a = Hoja a | Nodo (Arbol a) (Arbol a)
  deriving Show

a_podar :: Arbol a -> Arbol a
a_podar (Nodo (Hoja _) (Hoja _)) = Hoja
a_podar (Nodo izq der) = Nodo (a_podar izq) (a_podar der)
a_podar hoja = hoja -}













































