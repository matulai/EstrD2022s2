{-	
------------------------------------------------------
||	PRÁCTICA N°6 ||PRIORITY QUEUE, MAP y MULTISET   ||
||	Alumno: Matias Laime					        ||
||	Fecha De Inicio: 29/09/2022				        ||
------------------------------------------------------
-}
import PRIORITY_QUEUE
import MAP
--import Persona
{-
instance Eq Persona where 
    p1 == p2 = edad p1 == edad p2

instance Ord Persona where
    p1 <= p2 = edad p1 <= edad p2

instance Show Persona where
    show p = "Persona { nombre <- "      ++ show (nombre p)
                    ++ ", apellido <- " ++ (apellido p)
                    ++ ", edad <- "     ++ show (edad p)
                    ++ " }"
-}
--1.|PRIOTIRY QUEUE|
--Ejercicio 2
heapSort :: Ord a => [a] -> [a]
--Costo: O(n^2)
heapSort [] = []
heapSort xs = minimum xs : heapSort (sinElElemento (minimum xs) xs)

sinElElemento :: Eq a => a -> [a] -> [a]
sinElElemento _ []     = []
sinElElemento x (y:ys) = if x == y
                            then ys
                            else y : sinElElemento x ys
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
--PRECONDICIÓN: Las claves de la lista deben aparecer en el map dado.
mapALista _ []     = []
mapALista m (k:ks) =  (k, valor (lookupM k m)) : mapALista m ks

valor :: Maybe v -> v
valor Nothing  = error "NO hay valor"
valor (Just v) = v

-- agruparEq :: Eq k => [(k, v)] -> Map k [v]
-- -- Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan 
-- -- la misma clave.
-- agruparEq []          = emptyM
-- agruparEq ((k,v):kvs) = agruparClaves (k,v) (agruparEq kvs)   

-- agruparClaves :: Eq k => (k,v) -> Map k v -> Map k [v]
-- agruparClaves (k,v) m = assocM k v m 

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
mergeMaps m1 m2 = unirMaps (keys m1) m1 m2 

unirMaps :: Eq k => [k] -> Map k v -> Map k v -> Map k v
unirMaps []     _  m2 = m2
unirMaps (k:ks) m1 m2 = if lookupM k m2 == Nothing 
                                then unirMaps ks m1 m2
                                else assocM k (valor (lookupM k m1)) (unirMaps ks m1 (deleteM k m2))