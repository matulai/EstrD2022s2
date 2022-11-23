module Sector(
  Sector,
  Barril(Comida, Oxigeno, Torpedo, Combustible),
  Componente(LanzaTorpedos, Motor, Almacen),
  crearS,
  sectorId,
  componentesS,
  tripulantesS,
  agregarT,
  agregarC
)
where

import Set

type Rango = String
type Nombre = String
type SectorId = String

data Barril = Comida | Oxigeno | Torpedo | Combustible deriving Show
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril] deriving Show

data Sector = S SectorId [Componente] (Set Nombre) deriving Show

-- instance Ord Tripulante where
--   (T _ r1 _) `compare` (T _ r2 _) = r1 `compare` r2
-- instance Eq Tripulante where
--   (T n1 _ _) == (T n2 _ _) = n1 == n2
-- O(1)

crearS :: SectorId -> Sector 
crearS sid = S sid [] emptyS

-- O(1)
sectorId :: Sector -> SectorId 
sectorId (S sid _ _) = sid

-- O(1)
componentesS :: Sector -> [Componente] 
componentesS (S _ c _) = c

-- O(1)
tripulantesS :: Sector -> Set Nombre 
tripulantesS (S _ _ t) = t

-- O(1)
agregarC :: Componente -> Sector -> Sector
agregarC c (S sid cs t) = S sid (c:cs) t

-- O(log T)
agregarT :: Nombre -> Sector -> Sector 
agregarT t (S sid cs ts) = S sid cs (addS t ts)


