data ArtistaMusical = Cantante Registro TipoCanto Trayecto | Instrumentista Instrumento Trayecto deriving (Show)

data Registro = Soprano | Contralto | Tenor | Baritono | Bajo deriving (Show)
data TipoCanto = Solista | Coral deriving (Show)
data Instrumento = Violin | Clarinete | Trompeta | Timbales deriving (Show)
type Trayecto = Int


trayectoria_musical :: ArtistaMusical -> Trayecto
trayectoria_musical (Cantante r tc t) = t
trayectoria_musical (Instrumentista i t) = t



instance Eq ArtistaMusical where
        (Cantante r tc t) == (Cantante r2 tc2 t2) = t==t2  
        (Instrumentista i t) == (Instrumentista i2 t2) = t==t2
          
          
instance Ord ArtistaMusical where
        (Cantante r tc t) <= (Instrumentista i1 t1) = t<=t1
        (Cantante r tc t) <= (Cantante r2 tc2 t1) = t<=t1
        (Instrumentista i t) <= (Instrumentista i1 t1) = t<=t1
        (Instrumentista i t) <= (Cantante r2 tc2 t1) = t<=t1
        
buscarSolistas :: [ArtistaMusical] -> Registro -> [ArtistaMusical]
buscarSolistas [] _ = []
buscarSolistas ((Cantante Soprano Solista t):xs) Soprano = (Cantante Soprano Solista t) : buscarSolistas xs Soprano
buscarSolistas ((Cantante Contralto Solista t):xs) Contralto = (Cantante Contralto Solista t) : buscarSolistas xs Contralto
buscarSolistas ((Cantante Tenor Solista t):xs) Tenor = (Cantante Tenor Solista t) : buscarSolistas xs Tenor
buscarSolistas ((Cantante Baritono Solista t):xs) Baritono = (Cantante Baritono Solista t) :  buscarSolistas xs Baritono
buscarSolistas ((Cantante Bajo Solista t):xs) Bajo = (Cantante Bajo Solista t) : buscarSolistas xs Bajo
buscarSolistas (x:xs) r = buscarSolistas xs r








