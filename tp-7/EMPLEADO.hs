module EMPLEADO
    (Empleado, empleado, cuil, incorporarSector, sectores)
where

type SectorId = Int
type CUIL = Int
data Empleado = E CUIL [SectorId]

{-
    INVARIANTE DE REPRESENTACIÓN: En E CUIL [SectorId]
        -
        -
    CASOS VALIDOS:
        -
    CASOS INVALIDOS:
        -
-}

empleado :: CUIL -> Empleado
-- Propósito: construye un empleado con dicho CUIL.
-- Costo: O(1)
empleado c = E c [] 

cuil :: Empleado -> CUIL
-- Propósito: indica el CUIL de un empleado.
-- Costo: O(1)
cuil (E c _) = c

incorporarSector :: SectorId -> Empleado -> Empleado
-- Propósito: incorpora un sector al conjunto de sectores en los que trabaja un empleado.
-- Costo: O(log S), siendo S la cantidad de sectores que el empleado tiene asignados.
incorporarSector id (E c ids) = E c (id:ids)

sectores :: Empleado -> [SectorId]
-- Propósito: indica los sectores en los que el empleado trabaja.
-- Costo: O(1)
sectores (E _ ids) = ids
