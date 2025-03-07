{-	
----------------------------------------------------------
||	PRÁCTICA N°7 || HEAPS Y BSTs                    ||
||	Alumno: Matias Laime			        ||
||	Fecha De Inicio: 06/10/2022		        ||
----------------------------------------------------------
-}
import EMPRESA
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

arbol = NodeT 50 (NodeT 45 (NodeT 40 EmptyT EmptyT) (NodeT 47 EmptyT EmptyT)) (NodeT 55 (NodeT 53 EmptyT EmptyT) (NodeT 57 EmptyT EmptyT))

--Ejercicio 2

belongsBST :: Ord a => a -> Tree a -> Bool
--Propósito: dado un BST dice si el elemento pertenece o no al árbol.
--Costo: O(log N)
belongsBST _ EmptyT           = False
belongsBST x (NodeT x1 t1 t2) = if x==x1
                                    then True
                                    else if x < x1
                                        then belongsBST x t1
                                        else belongsBST x t2

insertBST :: Ord a => a -> Tree a -> Tree a
-- Propósito: dado un BST inserta un elemento en el árbol.
-- Costo: O(log N)
insertBST x EmptyT           = NodeT x EmptyT EmptyT
insertBST x (NodeT x1 t1 t2) = if x==x1
                                    then NodeT x t1 t2
                                    else if x < x1
                                        then NodeT x1 (insertBST x t1) t2
                                        else NodeT x1 t1 (insertBST x t2)            

deleteBST :: Ord a => a -> Tree a -> Tree a
-- Propósito: dado un BST borra un elemento en el árbol.
-- Costo: O(log N)                                
deleteBST x EmptyT           = EmptyT
deleteBST x (NodeT x1 t1 t2) = if x==x1
                                    then armarBST t1 t2 
                                    else if x < x1 
                                        then NodeT x1 (deleteBST x t1) t2
                                        else NodeT x1 t1 (deleteBST x t2)

armarBST :: Ord a => Tree a -> Tree a -> Tree a
armarBST EmptyT t2 = t2
armarBST t1     t2 = NodeT (maxEnArbol t1) (delArbolMax t1) t2 

maxEnArbol :: Ord a => Tree a -> a
maxEnArbol (NodeT x _ EmptyT) = x
maxEnArbol (NodeT x t1 t2)    = maxEnArbol t2

delArbolMax :: Ord a => Tree a -> Tree a
delArbolMax (NodeT _ EmptyT EmptyT) = EmptyT
delArbolMax (NodeT x t1 t2)         = NodeT x t1 (delArbolMax t2)

splitMinBST :: Ord a => Tree a -> (a, Tree a)
-- Propósito: dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
-- Costo: O(log N)
splitMinBST (NodeT x EmptyT t2) = (x, t2)
splitMinBST (NodeT x t1 t2)     = let (y, t1') = splitMinBST t1
                                    in (x, NodeT x t1' t2)

splitMaxBST :: Ord a => Tree a -> (a, Tree a)
-- Propósito: dado un BST devuelve un par con el máximo elemento y el árbol sin el mismo.
-- Costo: O(log N)
splitMaxBST (NodeT x t1 EmptyT) = (x, t1)
splitMaxBST (NodeT x t1 t2)     = let (y, t2') = splitMaxBST t2
                                    in (x, NodeT x t1 t2')

esBST :: Tree a -> Bool
-- Propósito: indica si el árbol cumple con los invariantes de BST.
-- Costo: O(N2)
esBST EmptyT          = True
esBST (NodeT x t1 t2) = esElMayorEn x t1 && esElMenorEn t2 && esBST t1 && esBST t2

esElMayorEn :: a -> Tree a -> Bool
esElMayorEn _ EmptyT           = True
esElMayorEn x (NodeT x' t1 t2) = x > x' && esElMayorEn x t1 && esElMayorEn x t2

esElMenorEn :: a -> Tree a -> Bool
esElMenorEn _ EmptyT           = True
esElMenorEn x (NodeT x' t1 t2) = x < x' && esElMenorEn x t1 && esElMenorEn x t2

elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
-- Propósito: dado un BST y un elemento, devuelve el máximo elemento que sea menor al
-- elemento dado. Costo: O(log N)
elMaximoMenorA x t = let y = minBST t
                        in if y < x
                                then Just y 
                                else Nothing

minBST :: Tree a -> a
--Costo:: O(log N)
minBST (NodeT x EmptyT _) = x 
minBST (NodeT _ t1 _)     = minBST t1   

elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
-- Propósito: dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al
-- elemento dado. Costo: O(log N)
elMinimoMayorA x t = let y = maxBST t
                        in if y < x
                                then Just y 
                                else Nothing

maxBST :: Tree a -> a
--Costo:: O(log N)
maxBST (NodeT x _ EmptyT) = x 
maxBST (NodeT _ _ t2)     = maxBST t2

balanceado :: Tree a -> Bool
-- Propósito: indica si el árbol está balanceado. Un árbol está balanceado cuando para cada
-- nodo la diferencia de alturas entre el subarbol izquierdo y el derecho es menor o igual a 1.
-- Costo: O(N^2)
balanceado EmptyT          = True
balanceado (NodeT _ t1 t2) = abs (highNodeT t1 - highNodeT t2) <= 1 && balanceado t1 && balanceado t2

--Ejercicio 3
{-
emptyM :: Map k v
Costo: O(1).
assocM :: Ord k => k -> v -> Map k v -> Map k v
Costo: O(log K).
lookupM :: Ord k => k -> Map k v -> Maybe v
Costo: O(log K).
deleteM :: Ord k => k -> Map k v -> Map k v
Costo: O(log K).
keys :: Map k v -> [k]
Costo: O(K).
-}
{-
Operaciones:                                                        costos:
        1. valuesM :: Eq k => Map k v -> [Maybe v]                          O(K log K)
        2. todasAsociadas :: Eq k => [k] -> Map k v -> Bool                 O(n^2)
        3. listToMap :: Eq k => [(k, v)] -> Map k v                         O(n log K)
        4. mapToList :: Eq k => Map k v -> [(k, v)]                         O(K log K)
        5. agruparEq :: Eq k => [(k, v)] -> Map k [v]                       O(K log K)
        6. incrementar :: Eq k => [k] -> Map k Int -> Map k Int             O(K^2)
        7. mergeMaps:: Eq k => Map k v -> Map k v -> Map k v                O(K log n)
-}
{-
        1.Se ejecuta un log K por cada clave, es decir, un total de K veces.
        2.No cambia el costo porque hago una opercion lineal por cada clave.
        3.Se ejecuta la operación assoc(log K) por cada clave-valor de la lista(de n elementos) dada.
        4.Se ejecuta la operación lookup(log K) por cada clave del map osea K veces.
        5.Se ejecuta la operación lookup(log K) por cada clave del map osea K veces.
        6.Se ejecuta la operacion elem(lineal) por cada clave del primer map, tambien se ejecuta la
          operación assoc(log K) y keys(K) lo cual son de un costo menor al lineal.
        7.Se ejecuta la operacion mapToList(K log K).
-}
--Ejercicio 4
comenzarCon :: [SectorId] -> [CUIL] -> Empresa
-- Propósito: construye una empresa con la información de empleados dada. Los sectores no
-- tienen empleados. Costo: calcular.
comenzarCon ids cs = agregarSectores ids (agregarCuils cs empresa) 

agregarCuils :: [CUIL] -> Empresa -> Empresa
agregarCuils []     emp = empresa
agregarCuils (c:cs) emp = agregarEmpleado [] c (agregarCuils cs emp)

agregarSectores :: [SectorId] -> Empresa -> Empresa
agregarSectores []       emp = emp
agregarSectores (id:ids) emp = agregarSector id (agregarSectores ids emp)

recorteDePersonal :: Empresa -> Empresa
-- Propósito: dada una empresa elimina a la mitad de sus empleados (sin importar a quiénes).
-- Costo: (E).
recorteDePersonal emp = let cs = todosLosCUIL emp
                        in recorte cs (div (length cs) 2) emp

recorte :: [CUIL] -> Int -> Empresa -> Empresa
recorte (c:cs) x emp = if length cs == x
                        then emp
                        else recorte cs x (borrarEmpleado c emp)


convertirEnComodin :: CUIL -> Empresa -> Empresa
-- Propósito: dado un CUIL de empleado le asigna todos los sectores de la empresa.
-- Costo: (I log I + S).
convertirEnComodin c emp = agregarEmpleado (todosLosSectores emp) c emp

esComodin :: CUIL -> Empresa -> Bool
-- Propósito: dado un CUIL de empleado indica si el empleado está en todos los sectores.
-- Costo: (E + S + I * (log S + E)) .
esComodin c emp = elem c (todosLosCUIL e) && estaEnTodosLosSectores (todosLosSectores e) (buscarPorCUIL c e) e

estaEnTodosLosSectores :: [SectorId] -> Empleado -> Empresa -> Bool
estaEnTodosLosSectores []       e _   = True
estaEnTodosLosSectores (id:ids) e emp = elem e (empleadosDelSector id) emp && estaEnTodosLosSectores ids e emp