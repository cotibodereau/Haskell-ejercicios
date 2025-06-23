data Vehiculo = Moto Color Potencia 
              | Auto Color EstiloAuto Potencia deriving (Show)
data Color = Rojo | Azul | Blanco | Gris deriving (Show)
data EstiloAuto = Sedan | Coupe deriving (Show)
type Potencia = Int 

potencia_vehiculo :: Vehiculo -> Potencia
potencia_vehiculo (Auto a b c) = c
potencia_vehiculo (Moto a c) = c

instance Eq Vehiculo where 
          vehiculo1 == vehiculo2 = potencia_vehiculo vehiculo1 == potencia_vehiculo vehiculo2

instance Ord Vehiculo where 
         vehiculo1 <= vehiculo2 = potencia_vehiculo vehiculo1 <= potencia_vehiculo vehiculo2


pintarCoupes :: [Vehiculo] -> Color -> [Vehiculo]
pintarCoupes [] c = []
pintarCoupes ((Moto a p):xs) c = (Moto a p) : pintarCoupes xs c
pintarCoupes ((Auto _ Coupe a):xs) c = (Auto c Coupe a) : pintarCoupes xs c
pintarCoupes ((Auto f g h) :xs) c = (Auto f g h) : pintarCoupes xs c

data ListaAsoc a b = Vacia | Nodo a b (ListaAsoc a b) deriving (Show)

la_esCotaInf :: (Eq a, Ord a) => ListaAsoc a b -> a -> Bool
la_esCotaInf Vacia _ = True
la_esCotaInf (Nodo a b la) x = (a >= x) && la_esCotaInf la x




