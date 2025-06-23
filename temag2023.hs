data ArticulosLibreria = Libro Categoria Editorial Titulo Precio | Agenda Marca AnioAgenda Precio | Cuaderno Marca Precio deriving (Show)

data Categoria = Literatura | Infantiles | AutoAyuda | Comics deriving (Show)
data Editorial = Altea | Minotauro | Panini deriving (Show)
data Marca = Monoblock | Papikra deriving (Show)


type Titulo = String
type Precio = Int
type AnioAgenda = Int


cuantosLibros :: [ArticulosLibreria] -> Categoria -> Int
cuantosLibros [] _ = 0
cuantosLibros ((Libro Literatura e t p):xs) Literatura = 1 + cuantosLibros xs Literatura
cuantosLibros ((Libro Infantiles e t p):xs) Infantiles = 1 + cuantosLibros xs Infantiles
cuantosLibros ((Libro AutoAyuda e t p):xs) AutoAyuda = 1 + cuantosLibros xs AutoAyuda
cuantosLibros ((Libro Comics e t p):xs) Comics = 1 + cuantosLibros xs Comics
cuantosLibros (x:xs) c =cuantosLibros xs c

mismaEdit :: Editorial -> Editorial -> Bool
mismaEdit Altea Altea = True
mismaEdit Minotauro Minotauro = True
mismaEdit Panini Panini = True
mismaEdit _ _ = False

mismoTit :: Titulo -> Titulo -> Bool
mismoTit t1 t2 | (t1==t2) = True
               | otherwise = False

mismaMar :: Marca -> Marca -> Bool
mismaMar Monoblock Monoblock = True
mismaMar Papikra Papikra = True
mismaMar _ _ = False


instance Eq ArticulosLibreria where
         (Libro c1 e1 t1 p1) == (Libro c2 e2 t2 p2) = (mismaEdit e1 e2) == (mismoTit t1 t2)
         (Agenda m1 a1 p1) == (Agenda m2 a2 p2) = (mismaMar m1 m2) == (a1==a2 && p1==p2)
         (Cuaderno m1 p1) == (Cuaderno m2 p2) = (mismaMar m1 m2) == (p1 == p2)


data NotasDeIngles = EvolucionEstudiante Nombre Nivel Primerp Segundop Final NotasDeIngles | NoHayMasEstudiantes

data Nivel = One | Two | Three
type Nombre = String
type Primerp = Int
type Segundop = Int
type Final = Int


nivelAlumno :: Nivel -> Primerp -> Segundop -> Final -> Bool
nivelAlumno One pp sp f = (pp>7 || sp>7) && f>5 
nivelAlumno Two pp sp f = (pp>7 || sp>7) && f>5 
nivelAlumno Three pp sp f = (pp>6 && sp>6) && f>7

pasaDeNivel :: NotasDeIngles -> String -> Bool
pasaDeNivel NoHayMasEstudiantes _ = False
pasaDeNivel (EvolucionEstudiante n1 niv pp sp f xs) nombre | (n1==nombre) = nivelAlumno niv pp sp f 










        
