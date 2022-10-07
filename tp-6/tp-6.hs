{-	
------------------------------------------------------
||	PRÁCTICA N°6 ||PRIORITY QUEUE, MAP y MULTISET   ||
||	Alumno: Matias Laime					        ||
||	Fecha De Inicio: 29/09/2022				        ||
------------------------------------------------------
-}
import PRIORITY_QUEUE
import MAPSinRepetidos
import PERSONA
import MULTISET

instance Eq Persona where 
    p1 == p2 = edad p1 == edad p2

instance Ord Persona where
    p1 <= p2 = edad p1 <= edad p2

instance Show Persona where
    show p = "Persona { nombre <- "     ++ show (nombre p)
                    ++ ", apellido <- " ++ show (apellido p)
                    ++ ", edad <- "     ++ show (edad p)
                    ++ " }"
--1.|PRIOTIRY QUEUE|
--Ejercicio 2
heapSort :: Ord a => [a] -> [a]
--Costo: O(n^2)
heapSort xs = armarListaConPq (armarPqConLista xs)

armarPqConLista :: Ord a => [a] -> PriorityQueue a
armarPqConLista []     = emptyPQ
armarPqConLista (x:xs) = insertPQ x (armarPqConLista xs)

armarListaConPq :: Ord a => PriorityQueue a -> [a]
armarListaConPq pq = if isEmptyPQ pq
                        then []
                        else findMinPQ pq : armarListaConPq (deleteMinPQ pq) 

--2.|MAP|
--Ejercicio 3
valuesM :: Eq k => Map k v -> [Maybe v]
--Propósito: obtiene los valores asociados a cada clave del map.
valuesM m = valoresM m (keys m)

valoresM :: Eq k => Map k v -> [k] -> [Maybe v]
valoresM m []     = []
valoresM m (k:ks) = lookupM k m : valoresM m ks

todasAsociadas :: Eq k => [k] -> Map k v -> Bool
--Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas ks m = estanTodasEn ks (keys m)

estanTodasEn :: Eq k => [k] -> [k] -> Bool
estanTodasEn []     _   = True
estanTodasEn (k:ks) k1s = elem k k1s && estanTodasEn ks k1s

listToMap :: Eq k => [(k, v)] -> Map k v
-- Propósito: convierte una lista de pares clave valor en un map.
listToMap []          = emptyM
listToMap ((k,v):kvs) = assocM k v (listToMap kvs)

mapToList :: Eq k => Map k v -> [(k, v)]
-- Propósito: convierte un map en una lista de pares clave valor.
mapToList m = mapALista m (keys m)

mapALista :: Eq k => Map k v -> [k] -> [(k,v)]
mapALista _ []     = []
mapALista m (k:ks) = case lookupM k m of
                        Just v  -> (k,v) : mapALista m ks
                        Nothing -> mapALista m ks

agruparEq :: Eq k => [(k, v)] -> Map k [v]
-- Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan 
-- la misma clave.
agruparEq []          = emptyM
agruparEq ((k,v):kvs) = case lookupM k (agruparEq kvs) of
                             Just v1  -> assocM k (v:v1) (agruparEq kvs)
                             Nothing  -> assocM k (v:[]) (agruparEq kvs)


incrementar :: Eq k => [k] -> Map k Int -> Map k Int
-- Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a cada 
-- número asociado con dichas claves.
incrementar ks m = incrementarM ks (keys m) 0

incrementarM :: Eq k => [k] -> [k] -> Int -> Map k Int
incrementarM _  []       _ = emptyM
incrementarM ks (k1:k1s) x = if elem k1 ks
                                then assocM k1 (x + 1) (incrementarM ks k1s (x + 1))
                                else assocM k1 x (incrementarM ks k1s (x + 1))

mergeMaps :: Eq k => Map k v -> Map k v -> Map k v
-- Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
-- una clave del primero existe en el segundo, es reemplazada por la del primero.
mergeMaps m1 m2 = unirMaps (mapToList m1) m1 m2 

unirMaps :: Eq k => [(k,v)] -> Map k v -> Map k v -> Map k v
unirMaps []          _  m2 = m2
unirMaps ((k,v):kvs) m1 m2 = case lookupM k m2 of 
                                Just v1 -> assocM k v (deleteM k (unirMaps kvs m1 m2))
                                Nothing -> unirMaps kvs m1 m2

{-
Operaciones:                                                        costos:
        1. valuesM :: Eq k => Map k v -> [Maybe v]                          O(n^2)
        2. todasAsociadas :: Eq k => [k] -> Map k v -> Bool                 O(n^2)
        3. listToMap :: Eq k => [(k, v)] -> Map k v                         O(n^2)
        4. mapToList :: Eq k => Map k v -> [(k, v)]                         O(n^2)
        5. agruparEq :: Eq k => [(k, v)] -> Map k [v]                       O(n^2)
        6. incrementar :: Eq k => [k] -> Map k Int -> Map k Int             O(n^2)
        7. mergeMaps:: Eq k => Map k v -> Map k v -> Map k v                O(n^2)
-}
{-
        1.No hay peor caso ya que el costo de operación depende de la cantidad de clave-valor del Map.
        2.No hay peor caso ya que el costo de operación depende de la cantidad de elementos de la lista.
        3.No hay peor caso ya que el costo de operación depende de la cantidad de clave-valor de la lista.
        4.No hay peor caso ya que el costo de operación depende de la cantidad de clave-valor del Map.
        5.No hay peor caso, ambos depende de lo mismo que es la cantidad de elementos del primero Map.
        6.El peor caso es que todas las claves de la lista dada existan en el Map dado.
        7.El peor caso es que todas las claves del primer Map existan en el segundo Map.
-}

--Ejercicio 5

indexar :: [a] -> Map Int a
-- Propósito: dada una lista de elementos construye un map que relaciona cada elemento con
-- su posición en la lista.
indexar []     = emptyM
indexar (x:xs) = assocM (length xs + 1) x (indexar xs)

ocurrencias :: String -> Map Char Int
-- Propósito: dado un string, devuelve un map donde las claves son los caracteres que aparecen
-- en el string, y los valores la cantidad de veces que aparecen en el mismo.
ocurrencias []     = emptyM
ocurrencias (x:xs) = case lookupM x (ocurrencias xs) of
                        Just v  -> assocM x (v + 1) (ocurrencias xs)
                        Nothing -> assocM x 1 (ocurrencias xs)

{-
    indexar: No hay peor caso ya que el costo de operación depende de la cantidad de elementos de la lista.
    ocurrencias: El peor caso es que todos sean repetidos.
-}

--3.|MULTISET|
-- Ejercicio 6
--1.
{-
emptyMS :: MultiSet a                                                       
    siempre se realiza solo una operación
addMS :: Ord a => a -> MultiSet a -> MultiSet a                             
    Se ejecutan operaciones lineales en la misma   pero no en forma de recorrido lo cual la hace lineal.
ocurrencesMS :: Ord a => a -> MultiSet a -> Int                             
    Se ejecuta una operacion lineal y no en forma de recorrido lo cual la hace lineal.
unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a                  
    Se ejecutan operaciones lineales en la misma y una se ejecuta por cada elemento de la lista
    lo que la hace cuadratica.
intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a           

multiSetToList :: MultiSet a -> [(a, Int)]                                  
    Se ejecutan operaciones lineales en la misma y una se ejecuta por cada elemento de la lista
    lo que la hace cuadratica.
-}
-- 2.
ocurrencias2 :: String -> MultiSet Char
ocurrencias2 []     = emptyMS
ocurrencias2 (c:cs) = addMS c (ocurrencias2 cs)