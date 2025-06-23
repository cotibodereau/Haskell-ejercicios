data Cancion = Tema Titulo Artista Genero Duracion | Publicidad Duracion
data Genero = Rock | Blues | Pop | Jazz

type Titulo = String
type Artista = String
type Duracion = Int

mismo_genero :: Genero -> Genero -> Bool
mismo_genero Rock Rock = True
mismo_genero Blues Blues = True
mismo_genero Pop Pop = True
mismo_genero Jazz Jazz = True
mismo_genero _ _ = False

duracion_de :: Cancion -> Duracion
duracion_de (Tema t a g d) = d


instance Eq Cancion where
         (Tema t a g d) == (Tema t1 a1 g1 d1) = (d==d1)

instance Ord Cancion where
        (Tema t a g d) <= (Tema t1 a1 g1 d1) = (d<=d1)

solo_genero :: [Cancion] -> Genero -> [Titulo]
solo_genero [] _ = []
solo_genero ((Tema t a Rock d):xs) Rock = t: solo_genero xs Rock
solo_genero ((Tema t a Blues d):xs) Blues = t: solo_genero xs Blues
solo_genero ((Tema t a Pop d):xs) Pop = t: solo_genero xs Pop
solo_genero ((Tema t a Jazz d):xs) Jazz = t: solo_genero xs Jazz
solo_genero (x:xs) g = solo_genero xs g

