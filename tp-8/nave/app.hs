import Nave
import Set
import Tripulante
import Sector


-- i) 
-- Propósito: Devuelve todos los sectores no vacíos (con tripulantes asignados).
sectores :: Nave -> Set SectorId
sectores n = sectores' (tripulantesN n) n

sectores' :: [Tripulante] -> Nave -> Set SectorId
sectores' [] n = emptyS
sectores' (t:ts) n = 
  unionS
    (sectoresAsignados (nombre t) n) 
    (sectores' ts n)




-- j) 
-- Propósito: Devuelve los tripulantes que no poseen sectores asignados.
-- O(T log T) por tripulantesN
-- + T por sinSectores
-- => (T * log T) + (T * 1) => T * (log T + 1) = (T log T)
sinSectoresAsignados :: Nave ->[Tripulante]
sinSectoresAsignados n = sinSectores (tripulantesN n)

-- O(n)
-- donde n es la cantidad de tripulantes provistos
sinSectores [] = []
sinSectores (t:ts) = 
  if sizeS (sectoresT t) == 0
    then t:(sinSectores ts)
    else (sinSectores ts)


-- k) 
-- Propósito: Devuelve todos los barriles de los sectores asignados de la nave.
barriles :: Nave -> [Barril]
barriles = undefined


nave1 = ingresarT "Jose" "002" $ ingresarT "Juan" "001" $  construir ["sectorX"]
nave2 = agregarASector [LanzaTorpedos, Motor 10] "sectorX" nave1