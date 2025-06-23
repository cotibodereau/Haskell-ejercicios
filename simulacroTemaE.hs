data Dedicacion = Simple | Semi | Full | Investigador deriving (Show)

misma_dedicacion :: Dedicacion -> Dedicacion -> Bool
misma_dedicacion Simple Simple = True
misma_dedicacion Semi Semi = True
misma_dedicacion Full Full = True
misma_dedicacion Investigador Investigador = True 
misma_dedicacion _ _ = False

type Cantidad = Int 

horas_trabajo :: Dedicacion -> Cantidad
horas_trabajo Simple = 10
horas_trabajo Semi = 20
horas_trabajo Full = 50
horas_trabajo Investigador = 60

data Persona = Decane Dedicacion | Docente Dedicacion | NoDocente Dedicacion deriving (Show)

instance Eq Dedicacion where dedicacion1 == dedicacion2 = horas_trabajo dedicacion1 == horas_trabajo dedicacion2

instance Ord Dedicacion where dedicacion1 <= dedicacion2 = horas_trabajo dedicacion1 <= horas_trabajo dedicacion2

----------------------------------------------
dedicadoA :: Persona -> Dedicacion
dedicadoA (Decane a) = a
dedicadoA (Docente b) = b
dedicadoA (NoDocente c) = c

solo_dedicacion :: [Persona] -> Dedicacion -> [Persona]
solo_dedicacion [] r = []
solo_dedicacion (x:xs) r | (dedicadoA x == r) = x : solo_dedicacion xs r
                         | otherwise = solo_dedicacion xs r

data ListaAsoc a b = Vacia | Nodo a b (ListaAsoc a b) deriving (Show)


la_mismo_valor :: ListaAsoc a b -> b -> ListaAsoc a b
la_mismo_valor Vacia x = Vacia
la_mismo_valor (Nodo a b la) x | (x==b) = (Nodo a b (la_mismo_valor la x))
                               | otherwise la_mismo_valor la x









