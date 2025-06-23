data ComponentePC = Micro MarcaMicro Nucleos MaxFrec | Ram MarcaRAM Gigas | PlacaMadre MarcaPMadre Chipset deriving (Show)

data MarcaMicro = Intel | AMD deriving (Show)
data Nucleos = DualCore | QuadCore | HexaCore | OctaCore deriving (Show)
type MaxFrec = Float

data MarcaRAM = Kingston | Markivision | Patriot deriving (Show)
type Gigas = Int

data MarcaPMadre = Asus | Asrock | MSI deriving (Show)
data Chipset = A630 | B650 | B660 | B760 deriving (Show)

cuantosMicros :: [ComponentePC] -> MarcaMicro -> Int
cuantosMicros [] _ = 0
cuantosMicros ((Micro Intel nu mf):xs) Intel = 1+ cuantosMicros xs Intel
cuantosMicros ((Micro AMD nu mf):xs) AMD = 1+ cuantosMicros xs AMD
cuantosMicros (x:xs) mm1 = cuantosMicros xs mm1


mismoNucleo :: Nucleos -> Nucleos -> Bool
mismoNucleo DualCore DualCore = True
mismoNucleo QuadCore QuadCore = True
mismoNucleo HexaCore HexaCore = True
mismoNucleo OctaCore OctaCore = True
mismoNucleo _ _ = False

mismoChipset :: Chipset -> Chipset -> Bool
mismoChipset A630 A630 = True
mismoChipset B650 B650 = True
mismoChipset B660 B660 = True
mismoChipset B760 B760 = True
mismoChipset _ _ = False


instance Eq ComponentePC where
         (Micro mm1 n1 mf1) == (Micro mm2 n2 mf2) = (mismoNucleo n1 n2)
         (Ram mr1 g1) == (Ram mr2 g2) = (g1==g2)
         (PlacaMadre mpm1 ch1) == (PlacaMadre mpm2 ch2) = (mismoChipset ch1 ch2)
         
data Playlist = Tema Titulo Rank Estado Duracion Playlist | SinTemas deriving (Show)
data Estado = Reproducido | SinReproducir deriving (Show)
type Duracion = Int
type Titulo = String
type Rank = Int


segundosRestantes :: Playlist -> Rank -> Duracion
segundosRestantes SinTemas _ = 0
segundosRestantes (Tema ti rk Reproducido d xs) r = segundosRestantes xs r
segundosRestantes (Tema ti rk SinReproducir d xs) r | (rk>=r) = d + segundosRestantes xs r
                                                    | otherwise = segundosRestantes xs r

estadoDelTema :: Titulo -> Playlist -> Maybe Estado
estadoDelTema _ SinTemas = Nothing
estadoDelTema ti (Tema tit rk e d xs) | (ti==tit) = Just e
                                      | otherwise = estadoDelTema ti xs





















