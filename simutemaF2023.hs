data ComponentePC = Micro MarcaMicro Nucleos MaxFrec | Ram MarcaRAM Gigas | PlacaMadre MarcaPMadre Chipset

data MarcaMicro = Intel | AMD
data Nucleos = DualCore | QuadCore | HexaCore | OctaCore
type MaxFrec = Float

data MarcaRam = Kingston | Markivision | Patriot
type Gigas = Int

data MarcaPMadre = Asus | Asrock | MSI
data Chipset = A630 | B650 | B660 | B760

cuantosMicros :: [ComponentePC] -> MarcaMicro -> Int
cuantosMicros [] _ = 0
cuantosMicros (Micro m n mf):xs) mm |(m==mm) = 1 + cuantosMicros (Micro m n mf) xs mm
