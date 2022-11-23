module Tripulante(
  Tripulante,
  crearT,
  nombre,
  asignarS,
  sectoresT,
  rango
)
where

import Set

type Rango = String
type Nombre = String
type SectorId = String

data Tripulante = T Nombre Rango (Set SectorId) deriving Show

instance Ord Tripulante where
  (T _ r1 _) `compare` (T _ r2 _) = r1 `compare` r2
instance Eq Tripulante where
  (T n1 _ _) == (T n2 _ _) = n1 == n2

--O(1)
crearT :: Nombre -> Rango -> Tripulante 
crearT n r = T n r emptyS

-- O(log S)
asignarS :: SectorId -> Tripulante -> Tripulante
asignarS si (T n r s) = T n r (addS si s)

-- O(1)
sectoresT :: Tripulante -> Set SectorId 
sectoresT (T n r s) = s

-- O(1)
nombre :: Tripulante -> String 
nombre (T n r s) = n

-- O(1)
rango :: Tripulante -> Rango 
rango (T n r s) = r
