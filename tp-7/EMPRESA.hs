module EMPRESA
    (Empresa, )
where
    
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
buscarPorCUIL c (Emp _ ce) = empleadoConCUIL c ce 

empleadoConCUIL :: CUIL -> Map CUIL Empleado -> Empleado
empleadoConCUIL 
empleadoConCUIL

empleadosDelSector :: SectorId -> Empresa -> [Empleado]
-- Propósito: indica los empleados que trabajan en un sector dado.
-- Costo: O(logS + E)

todosLosCUIL :: Empresa -> [CUIL]
-- Propósito: indica todos los CUIL de empleados de la empresa.
-- Costo: O(E)

todosLosSectores :: Empresa -> [SectorId]
-- Propósito: indica todos los sectores de la empresa.
-- Costo: O(S)

agregarSector :: SectorId -> Empresa -> Empresa
-- Propósito: agrega un sector a la empresa, inicialmente sin empleados.
-- Costo: O(logS)

agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
-- Propósito: agrega un empleado a la empresa, en el que trabajará en dichos sectores y tendrá
-- el CUIL dado. Costo: calcular.

agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
-- Propósito: agrega un sector al empleado con dicho CUIL.
-- Costo: calcular.

borrarEmpleado :: CUIL -> Empresa -> Empresa
-- Propósito: elimina al empleado que posee dicho CUIL.
-- Costo: calcular.