module EMPRESA
    (Empresa, empresa, buscarPorCUIL, empleadosDelSector, todosLosCUIL, todosLosSectores,
     agregarSector, agregarEmpleado, agregarASector, borrarEmpleado)
where

import EMPLEADO
import MAPSinRepetidos
import SETSinRepetidos
type SectorId = Int
type CUIL = Int
data Empresa = Emp (Map SectorId (Set Empleado)) (Map CUIL Empleado)

instance Eq Empleado where 
    e1 == e2 = cuil e1 == cuil e2

instance Ord Empleado where
    e1 <= e2 = cuil e1 <= cuil e2

-- instance Show Persona where
--     show p = "Persona { nombre <- "     ++ show (nombre p)
--                     ++ ", apellido <- " ++ show (apellido p)
--                     ++ ", edad <- "     ++ show (edad p)
--                     ++ " }"

{-
    INVARIANTE DE REPRESENTACIÓN: En E (Map SectorId (Set Empleado)) (Map CUIL Empleado)
        -El id de cada sector es único.
        -El CUIL de cada empleado es único en cada sector y puede estar asignado a múltiples sectores.
        -Los números de CUIL deben ser mayores a 100 y menores a 200.(tenemos un cap de 100 empleados)
        -Los números de id de los sectores deben ser mayores a 0 y menores a 50. 
    CASOS VALIDOS:
        -
    CASOS INVALIDOS:
        -
-}
{-
Opereciones:                                                            costos:
            empresa :: Empresa                                                 Costo: O(1)
            buscarPorCUIL :: CUIL -> Empresa -> Empleado                       Costo: O(log E)
            empleadosDelSector :: SectorId -> Empresa -> [Empleado]            Costo: O(logS + E)
            todosLosCUIL :: Empresa -> [CUIL]                                  Costo: O(E)
            todosLosSectores :: Empresa -> [SectorId]                          Costo: O(S)
            agregarSector :: SectorId -> Empresa -> Empresa                    Costo: O(logS)
            agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa        Costo: O(I log I)
            agregarASector :: SectorId -> CUIL -> Empresa -> Empresa           Costo: O(log C)
            borrarEmpleado :: CUIL -> Empresa -> Empresa                       Costo: O(I log I + log C)
-}

empresa :: Empresa
-- Propósito: construye una empresa vacía.
-- Costo: O(1)
empresa = Emp emptyM emptyM

buscarPorCUIL :: CUIL -> Empresa -> Empleado
-- Propósito: devuelve el empleado con dicho CUIL.
-- Costo: O(log E)
buscarPorCUIL c (Emp _ ce) = case lookupM c ce of
                                Just e  -> e 
                                Nothing -> error "No existe empleado con tal CUIL" 

empleadosDelSector :: SectorId -> Empresa -> [Empleado]
-- Propósito: indica los empleados que trabajan en un sector dado.
-- Costo: O(log S + E)
empleadosDelSector id (Emp se _) = case lookupM id se of
                                    Just es -> setToList es
                                    Nothing -> []

todosLosCUIL :: Empresa -> [CUIL]
-- Propósito: indica todos los CUIL de empleados de la empresa.
-- Costo: O(E)
todosLosCUIL (Emp se ce) = keys ce

todosLosSectores :: Empresa -> [SectorId]
-- Propósito: indica todos los sectores de la empresa.
-- Costo: O(S)
todosLosSectores (Emp se ce) = keys se 

agregarSector :: SectorId -> Empresa -> Empresa
-- Propósito: agrega un sector a la empresa, inicialmente sin empleados.
-- Costo: O(log S)
agregarSector id (Emp se ce) = Emp (assocM id emptyS se) ce

agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
-- Propósito: agrega un empleado a la empresa, en el que trabajará en dichos sectores y tendrá
-- el CUIL dado. Costo: (I log I) siendo I la cantidad de sectores de la empresa.
agregarEmpleado ids c (Emp se ce) = let e = incorporarSectores ids (empleado c)
                                    in Emp (empleadoASectores e ids se) (assocM c e ce)

empleadoASectores :: Empleado -> [SectorId] -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
empleadoASectores e []       _  = emptyM  
empleadoASectores e (id:ids) se = case lookupM id se of
                                    Just es -> assocM id (addS e es) (empleadoASectores e ids se)
                                    Nothing -> empleadoASectores e ids se

incorporarSectores :: [SectorId] -> Empleado -> Empleado
incorporarSectores []       e = e 
incorporarSectores (id:ids) e = incorporarSector id e

agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
-- Propósito: agrega un sector al empleado con dicho CUIL.
-- Costo: (log C) siendo C la cantidad de cuils de la empresa.
agregarASector id c (Emp se ce) = case lookupM c ce of
                                    Just e  -> Emp se (assocM c (incorporarSector id e) ce)
                                    Nothing -> Emp se ce

borrarEmpleado :: CUIL -> Empresa -> Empresa
-- Propósito: elimina al empleado que posee dicho CUIL.
-- Costo: (I log I + log C) siendo I la cantidad de id de sectores en la empresa y C la cantidad de Cuils.
borrarEmpleado c (Emp se ce) = Emp (eliminarDeSectores (buscarPorCUIL c se) (keys se) se) (deleteM c ce)

eliminarDeSectores :: Empleado -> [SectorId] -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
eliminarDeSectores e []       m = m
eliminarDeSectores e (id:ids) m = case lookupM id m of
                                    Just es -> assocM id (removeS e es) (eliminarDeSectores e ids m)
                                    Nothing -> eliminarDeSectores e ids m