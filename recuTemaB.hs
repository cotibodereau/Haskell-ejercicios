data Vehiculo = Moto Color Potencia | Auto Color EstiloAuto Potencia deriving (Show)

data Color = Rojo | Azul | Blanco | Gris deriving (Show)
data EstiloAuto = Sedan | Coupe deriving (Show)

type Potencia = Int

potencia_vehiculo :: Vehiculo -> Potencia
potencia_vehiculo (Moto c p) = p
potencia_vehiculo (Auto c e p) = p

{- instance Ord Vehiculo where  
(<=) vehiculo1 vehiculo2 = potencia_vehiculo vehiculo1 (<=) potencia_vehiculo vehiculo2 -}

pintarCoupes :: [Vehiculo] -> Color -> [Vehiculo]
pintarCoupes [] c1 = []
pintarCoupes ((Auto c Coupe p):xs) c1 = (Auto c1 Coupe p) : pintarCoupes xs c1
pintarCoupes (x:xs) c1 = x : pintarCoupes xs c1

{- *Main> pintarCoupes [(Moto Azul 123), (Auto Rojo Sedan 234), (Auto Gris Coupe 324)] Blanco 
[Moto Azul 123,Auto Rojo Sedan 234,Auto Blanco Coupe 324] -}

type ListaAsoc a b = [(a,b)]

la_es_cota_inf :: ListaAsoc a b -> a -> Bool

