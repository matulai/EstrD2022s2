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
            agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa        
            agregarASector :: SectorId -> CUIL -> Empresa -> Empresa           
            borrarEmpleado :: CUIL -> Empresa -> Empresa                       
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
empleadosDelSector id (E se _) = case lookupM id se of
                                    Just es -> setToList es
                                    Nothing -> []

todosLosCUIL :: Empresa -> [CUIL]
-- Propósito: indica todos los CUIL de empleados de la empresa.
-- Costo: O(E)
todosLosCUIL (E se ce) = keys ce

todosLosSectores :: Empresa -> [SectorId]
-- Propósito: indica todos los sectores de la empresa.
-- Costo: O(S)
todosLosSectores (E se ce) = keys se 

agregarSector :: SectorId -> Empresa -> Empresa
-- Propósito: agrega un sector a la empresa, inicialmente sin empleados.
-- Costo: O(log S)
agregarSector id (E se ce) = E (assocM id emptyS se) ce

agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
-- Propósito: agrega un empleado a la empresa, en el que trabajará en dichos sectores y tendrá
-- el CUIL dado. Costo: calcular.
agregarEmpleado ids c (E se ce) = let e = incorporarSectores ids (empleado c)
                                  in E (empleadoASectores e ids se) (assocM c e ce)

empleadoASectores :: Empleado -> [SectorId] -> Map SectorId Set Empleado -> Map SectorId Set Empleado
empleadoASectores e []       _  = emptyM  
empleadoASectores e (id:ids) se = case lookupM id se of
                                    Just es -> assocM id (adds e es) (empleadoASectores e ids se)
                                    Nothing -> empleadoASectores e ids se

incorporarSectores :: [SectorId] -> Empleado -> Empleado
incorporarSectores []       e = e 
incorporarSectores (id:ids) e = incorporarSector id e

agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
-- Propósito: agrega un sector al empleado con dicho CUIL.
-- Costo: calcular.
agregarASector id c (E se ce) = case lookupM c ce of
                                    Just e  -> E se (assocM c (incorporarSector id e) ce)
                                    Nothing -> E se ce

borrarEmpleado :: CUIL -> Empresa -> Empresa
-- Propósito: elimina al empleado que posee dicho CUIL.
-- Costo: calcular.
borrarEmpleado c (E se ce) = E (eliminarDeSectores (buscarPorCUIL c) (keys se) se) (deleteM c)

eliminarDeSectores :: Empleado -> [SectorId] -> Map SectorId Set Empleado -> Map SectorId Set Empleado
eliminarDeSectores e []       m = m
eliminarDeSectores e (id:ids) m = case lookupM id m of
                                    Just es -> assocM id (removeS e es) (eliminarDeSectores e ids m)
                                    Nothing -> eliminarDeSectores e ids m