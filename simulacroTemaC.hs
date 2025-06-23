data Deporte = Futbol | Basket | Tenis | Valorant | Dota2 deriving (Eq, Show)
type MinJugadores = Int

minimaCantidad :: Deporte -> MinJugadores
minimaCantidad Tenis = 2
minimaCantidad Futbol = 22
minimaCantidad Basket = 10
minimaCantidad Valorant = 10
minimaCantidad Dota2 = 10

--- 2
{- type NombrePersona = String
data PracticoDeporte = Niguna | AgregaDeporte Deporte NombrePersona PracticoDeporte deriving (Eq)
                     
deporte :: PracticoDeporte -> Deporte -> NombrePersona -> Bool
deporte Ninguna d1 np1 = False
deporte (AgregaDeporte d1 np1 pd) d2 n2 | (d1 == d2) && (np1 == np2) = True
                                         | otherwise = deporte pd d2 np2 -}
                                         
type EquipoFavorito = String

