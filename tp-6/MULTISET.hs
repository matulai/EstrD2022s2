module MULTISET
    (MultiSet, emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList)
where
import MAPSinRepetidos

data MultiSet a = MS (Map a Int)
--Utilizando MAPSinRepetidos
{-
    INVARIANTE DE REPRESENTACIÓN: En M [(k,v)]
        -
        -
    CASOS VALIDOS:
        -
    CASOS INVALIDOS:
        -
-}
{-
Operaciones:                                                        costos:
emptyMS :: MultiSet a                                                       O(1)
addMS :: Ord a => a -> MultiSet a -> MultiSet a                             O(N)
ocurrencesMS :: Ord a => a -> MultiSet a -> Int                             O(N)
unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a                  O(n^2)
intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a           O()
multiSetToList :: MultiSet a -> [(a, Int)]                                  O(n^2)
-}

emptyMS :: MultiSet a
-- Propósito: denota un multiconjunto vacío.
emptyMS = MS emptyM

addMS :: Ord a => a -> MultiSet a -> MultiSet a
-- Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al
-- multiconjunto.
addMS x (MS m) = case lookupM x m of
                    Just v  -> MS (assocM x (v + 1) m)
                    Nothing -> MS (assocM x 1 m)

ocurrencesMS :: Ord a => a -> MultiSet a -> Int
-- Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese
-- elemento en el multiconjunto.
ocurrencesMS x (MS m) = case lookupM x m of 
                            Just v  -> v
                            Nothing -> 0

unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a 
-- Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de
-- ambos multiconjuntos.
unionMS (MS m) ms = unirMS (keys m) ms

unirMS :: Ord a => [a] -> MultiSet a -> MultiSet a
unirMS []     ms = ms
unirMS (k:ks) ms = addMS k (unirMS ks ms)


intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a 
-- Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos
-- multiconjuntos tienen en común.
intersectionMS = undefined

multiSetToList :: Eq a => MultiSet a -> [(a, Int)]
-- Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y
-- su cantidad de ocurrencias.
multiSetToList (MS m) = mapALista m (keys m) 

mapALista :: Eq k => Map k Int -> [k] -> [(k,Int)]
mapALista _ []     = []
mapALista m (k:ks) = case lookupM k m of
                        Just v  -> (k,v) : mapALista m ks
                        Nothing -> mapALista m ks