data ProductoDeVivero = Planta TipoPlanta Ubicacion Agua Precio| BolsaSemilla TipoSemilla Peso Precio | Maceta TipoMaceta Forma Precio

data TipoPlanta = PlenoSol | MediaSombra | Floral | Frutal
data Ubicacion = Exterior | Interior
data Agua = MuyResistente | Resistente | NadaResistente
data TipoSemilla = Hortalizas | Aromaticas | Cesped
data TipoMaceta = Terracota | FibroCemento | Plastico
data Forma = Cuadrada | Redonda

type Precio = Int
type Peso = Float

cuantasPlantas :: [ProductoDeVivero] -> TipoPlanta -> Int
cuantasPlantas [] _ = 0
cuantasPlantas ((Planta PlenoSol u a p):xs) PlenoSol = 1 + cuantasPlantas xs PlenoSol
cuantasPlantas ((Planta MediaSombra u a p):xs) MediaSombra = 1 + cuantasPlantas xs MediaSombra
cuantasPlantas ((Planta Floral u a p):xs) Floral = 1 + cuantasPlantas xs Floral
cuantasPlantas ((Planta Frutal u a p):xs) Frutal = 1 + cuantasPlantas xs Frutal
cuantasPlantas (x:xs) pv = cuantasPlantas xs pv


tipoPlanta :: TipoPlanta -> TipoPlanta -> Bool
tipoPlanta PlenoSol PlenoSol = True
tipoPlanta MediaSombra MediaSombra = True
tipoPlanta Floral Floral = True
tipoPlanta Frutal Frutal = True
tipoPlanta _ _ = False

mismaubi :: Ubicacion -> Ubicacion -> Bool
mismaubi Exterior Exterior = True
mismaubi Interior Interior = True
mismaubi _ _ = False

tipoMaceta :: TipoMaceta -> TipoMaceta -> Bool
tipoMaceta Terracota Terracota = True
tipoMaceta FibroCemento FibroCemento = True
tipoMaceta Plastico Plastico = True
tipoMaceta _ _ = False

instance Eq ProductoDeVivero where
         (Planta tp u a p) == (Planta tp1 u1 a1 p1) = (tipoPlanta tp tp1) == (mismaubi u u1)
         (BolsaSemilla ts pe pr) == (BolsaSemilla ts1 pe1 pr1) = (pe==pe1) == (pr==pr1)
         (Maceta tm f p) == (Maceta tm1 f1 p1) = (tipoMaceta tm tm1)
         
data RegistroPlanta = DatoPlanta EstadoHojas Altura CantidadTotal CantidadFlorecidas Edad RegistroPlanta | NoDato

data EstadoHojas = MuchasQuemadas | PocasQuemadas | Saludables deriving (Show)
type Altura = Int
type CantidadTotal = Int
type CantidadFlorecidas = Int
type Edad = Int

estadoPlantas :: EstadoHojas -> Altura -> CantidadTotal -> CantidadFlorecidas -> Edad -> Bool
estadoPlantas MuchasQuemadas a ct cf e = (ct<10)
estadoPlantas PocasQuemadas a ct cf e = (a>40)
estadoPlantas Saludables a ct cf e = (a>40)

trasplantar :: RegistroPlanta -> Int -> Bool
trasplantar NoDato _ = False
trasplantar (DatoPlanta eh a ct cf e xs) num | (e>=num)= estadoPlantas eh a ct cf e
                                             | otherwise = False

devolverEstado :: RegistroPlanta -> Int -> Maybe EstadoHojas
devolverEstado NoDato _ = Nothing
devolverEstado (DatoPlanta eh a ct cf e xs) meses | (e>=meses) = Just eh
                                                  | otherwise = Nothing
                                                  
















         
