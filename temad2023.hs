data Perecedero = Leche TipoDeLeche UsoDeLeche Precio | Carne Corte Peso Precio | Queso TipoDeQueso Peso Precio
data TipoDeLeche = Descremada | Entera | Condenzada | Polvo
data UsoDeLeche = Bebida | Preparaciones
data Corte = Bife | Molida | Pulpa
data TipoDeQueso = Barra | Cremoso | Duro

type Precio = Int
type Peso = Float

cuantosQuesos :: [Perecedero] -> TipoDeQueso -> Int
cuantosQuesos [] _ = 0
cuantosQuesos ((Queso Barra pes pre):xs) Barra = 1 + cuantosQuesos xs Barra
cuantosQuesos ((Queso Cremoso pes pre):xs) Cremoso = 1 + cuantosQuesos xs Cremoso
cuantosQuesos ((Queso Duro pes pre):xs) Duro = 1 + cuantosQuesos xs Duro
cuantosQuesos (x:xs) q = cuantosQuesos xs q

tipoleche :: TipoDeLeche -> TipoDeLeche -> Bool
tipoleche Descremada Descremada = True
tipoleche Entera Entera = True
tipoleche Condenzada Condenzada = True
tipoleche Polvo Polvo = True
tipodeleche _ _ = False

usodeleche :: UsoDeLeche -> UsoDeLeche -> Bool
usodeleche Bebida Bebida = True
usodeleche Preparaciones Preparaciones = True
usodeleche _ _ = False

mismocorte :: Corte -> Corte -> Bool
mismocorte Bife Bife = True
mismocorte Molida Molida = True
mismocorte Pulpa Pulpa = True
mismocorte _ _ = False

tipoqueso :: TipoDeQueso -> TipoDeQueso -> Bool
tipoqueso Barra Barra = True
tipoqueso Cremoso Cremoso = True
tipoqueso Duro Duro = True
tipoqueso _ _ = False

instance Eq Perecedero where
         (Leche tl1 ud1 p1) == (Leche tl2 ud2 p2) = (tipoleche tl1 tl2) == (usodeleche ud1 ud2)
         (Carne co1 pe1 pr1) == (Carne co2 pe2 pr2) = (mismocorte co1 co2)
         (Queso tp1 pe1 pr1) == (Queso tp2 pe2 pr2) = (tipoqueso tp1 tp2)
         
data NotasDelClub = EvolJugador Nombre Division Defensiva Ataque Pases NotasDelClub | NoHayMasJugadores

data Division = Septima | Sexta | Quinta 
type Nombre = String
type Defensiva = Int
type Ataque = Int
type Pases = Int

niveljugador :: Division -> Defensiva -> Ataque -> Pases -> Bool
niveljugador Septima d a p = (d>7 || a>7) && p>5
niveljugador Sexta d a p = (d>7 || a>7) && p>5
niveljugador Quinta d a p = (d>6 && a>6) && p>7


pasaDeDivision :: NotasDelClub -> String -> Bool
pasaDeDivision NoHayMasJugadores _ = False
pasaDeDivision (EvolJugador n div def at pa xs) nombre | (n==nombre) = niveljugador div def at pa 
                                                       | otherwise = False










