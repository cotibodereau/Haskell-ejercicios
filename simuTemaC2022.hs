data Ropa = Camisa | Pantalon | Pollera | Short deriving (Show)

misma_ropa :: Ropa -> Ropa -> Bool
misma_ropa Camisa Camisa = True
misma_ropa Pantalon Pantalon = True
misma_ropa Pollera Pollera = True
misma_ropa Short Short = True
misma_ropa _ _ = False

data Prenda = ConTalle Talle Ropa | TalleUnico Ropa deriving (Show)
type Talle = Int

valor_talle :: Prenda -> Int
valor_talle (ConTalle t r) = t
valor_talle (TalleUnico r) = 0

{- instance Ord Prenda where
  (<=) t1 t2 = valor_talle t1 <= valor_talle t2 -}
  
solo_con_talle :: [Prenda] -> Ropa -> [Talle]
solo_con_talle [] r1 = []
solo_con_talle ((ConTalle t r):xs) r1 | misma_ropa r r1 = valor_talle (ConTalle t r) : solo_con_talle xs r1
                                      | otherwise = solo_con_talle xs r1
solo_con_talle ((TalleUnico r):xs) r1 = solo_con_talle xs r1
