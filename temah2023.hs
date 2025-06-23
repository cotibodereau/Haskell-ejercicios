{-1--
a) 1
b) 2
c) 2
d) 4 -} 


{- data Astro = Estrella Luminosidad Temperatura Nombre | Planeta Estructura NumSat DistEstrella deriving (Show)

data Luminosidad = Supergigante | Gigante | SecuenciaPrincipal | Enana deriving (Show)
data Temperatura = A | B | F | G | K | M | O deriving (Show)
type Nombre = String
data Estructura = Rocoso | Gaseoso deriving (Show)
type NumSat = Int
type DistEstrella = Float

esPlaneta :: Astro -> Bool
esPlaneta (Planeta e n d) = True
esPlaneta _ = False

numeroSat :: Astro -> Int
numeroSat (Planeta e n d) = n
numeroSat _ = 0

masSatelites :: [Astro] -> Int -> [Astro]
masSatelites [] _ = []
masSatelites (x:xs) n1 | esPlaneta x && (numeroSat x >= n1) = x : masSatelites xs n1
                       | otherwise = masSatelites xs n1


lumiEstrella :: Astro -> Int
lumiEstrella (Estrella Supergigante temp nom) = 3
lumiEstrella (Estrella Gigante temp nom) = 2
lumiEstrella (Estrella SecuenciaPrincipal temp nom) = 1
lumiEstrella (Estrella Enana temp nom) = 0

tempEstrella :: Astro -> Int
tempEstrella (Estrella lu O nom) = 0
tempEstrella (Estrella lu B nom) = 1
tempEstrella (Estrella lu A nom) = 2
tempEstrella (Estrella lu F nom) = 3
tempEstrella (Estrella lu G nom) = 4
tempEstrella (Estrella lu K nom) = 5
tempEstrella (Estrella lu M nom) = 6


instance Eq Astro where
         (Estrella lu1 temp1 nom1) > (Estrella lu2 temp2 nom2) = (lumiEstrella Estrella lu1 temp1 nom1) > (lumiEstrella Estrella lu2 temp2 nom2) -}

data NotasDeIngles = EvolucionDelEstudiante Nombre Nivel Parcial1 Parcial2 Final NotasDeIngles | NoHayMasEstudiantes

type Nombre = String
type Parcial1 = Int
type Parcial2 = Int
type Final = Int
data Nivel = Uno | Dos | Tres

nivelAlumno :: Nivel -> Parcial1 -> Parcial2 -> Final -> Bool
nivelAlumno Uno p1 p2 f = (p1>8 || p2 >8) && f>6 
nivelAlumno Dos p1 p2 f = (p1>8 || p2 >8) && f>6
nivelAlumno Tres p1 p2 f = (p1>5 && p2 >5) && f>6  

pasaDeNivel :: NotasDeIngles -> String -> Bool
pasaDeNivel NoHayMasEstudiantes _ = False
pasaDeNivel (EvolucionDelEstudiante nom n p1 p2 f xs) bre | (nom==bre) = nivelAlumno n p1 p2 f
                                                          | otherwise = False
                                                          
devolverNotaFinal :: NotasDeIngles -> String -> Maybe Int 
devolverNotaFinal NoHayMasEstudiantes _ = Nothing
devolverNotaFinal (EvolucionDelEstudiante no ni p1 p2 f xs) nombre | (no==nombre) = Just f
                                                                   | otherwise = Nothing


















