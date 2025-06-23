data Forma = Piedra | Papel | Tijera

le_gana :: Forma -> Forma -> Bool
le_gana Piedra Tijera = True
le_gana Papel Piedra = True
le_gana Tijera Papel = True
le_gana _ _ = False

type Nombre = String

data Jugador = Mano Nombre Forma

ganador :: Jugador -> Jugador -> Maybe Nombre
ganador (Mano n1 f1) (Mano n2 f2) | le_gana f1 f2 = Just n1
                                  | le_gana f2 f1 = Just n2
                                  | otherwise = Nothing
                  
mismaforma :: Forma -> Forma -> Bool 
mismaforma Tijera Tijera = True
mismaforma Papel Papel = True
mismaforma Piedra Piedra = True
mismaforma _ _ = False


quien_jugo :: Forma -> [Jugador] -> [Nombre]
quien_jugo f [] = []
quien_jugo f ((Mano n1 f1):xs) | mismaforma f f1 = n1 : quien_jugo f xs
                               | otherwise = quien_jugo f xs


data NotaMusical = Do | Re | Mi | Fa | Sol | La | Si
data Figura = Negra | Corchea
data Melodia = Entonar NotaMusical Figura Melodia | Vacia

contar_tiempos :: Melodia -> Int
contar_tiempos Vacia = 0
contar_tiempos (Entonar nota Negra m ) = 2 + contar_tiempos m
contar_tiempos (Entonar nota Corchea m ) = 1 + contar_tiempos m

{- data Arbol a = Nodo a (Arbol a) (Arbol a)| Vacio


arbol_sum :: Arbol Int -> Arbol Int -> Arbol Int
arbol_sum Vacio b = b
arbol_sum a Vacio = a
arbol_sum (Nodo x izqA derA) (Nodo y izqB derB) = Nodo (x + y) (arbol_sum izqA izqB) (arbol_sum derA derB) -}










