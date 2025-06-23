data Forma = Piedra | Papel | Tijera deriving (Show)

le_gana :: Forma -> Forma -> Bool
le_gana Piedra Papel = False
le_gana Piedra Tijera  = True
le_gana Piedra Piedra = False
le_gana Papel Tijera = False
le_gana Papel Piedra = True
le_gana Papel Papel = False
le_gana Tijera Piedra = False
le_gana Tijera Papel = True
le_gana Tijera Tijera = False

type Nombre = String
data Jugador = Mano Nombre Forma deriving (Show)

ganador :: Jugador -> Jugador -> Maybe Nombre
ganador (Mano n1 f1) (Mano n2 f2) | le_gana f1 f2 = Just n1
                                  | le_gana f2 f1 = Just n2
                                  | otherwise = Nothing 

mismaforma :: Forma -> Forma -> Bool
mismaforma Piedra Piedra = True
mismaforma Tijera Tijera = True
mismaforma Papel Papel = True
mismaforma _ _ = False

quien_jugo :: Forma -> [Jugador] -> [Nombre]
quien_jugo f [] = []
quien_jugo f ((Mano n1 f1):xs) | mismaforma f f1 = n1 : quien_jugo f xs
                               | otherwise = quien_jugo f xs

data NotaMusical = Do | Re | Mi | Fa | Sol | La | Si
data Figura = Negra | Corchea deriving (Eq)
data Melodia = Entonar NotaMusical Figura Melodia
             | Vacia


contar_tiempos :: Melodia -> Int
contar_tiempos Vacia = 0
contar_tiempos (Entonar nota Negra m ) = 2 + contar_tiempos m
contar_tiempos (Entonar nota Corchea m ) = 1 + contar_tiempos m  


