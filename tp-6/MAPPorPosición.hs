module MAPPorPosición
    (Map, emptyM, assocM, lookupM, deleteM, keys)
where

data Map k v = M [k] [v]
 
{-
    INVARIANTE DE REPRESENTACIÓN: En M [k] [v]
        -
        -
    CASOS VALIDOS:
        -
    CASOS INVALIDOS:
        -
-}
{-
Opereciones:                                                    costos:
            emptyM :: Map k v                                          O(1)
            assocM :: Eq k => k -> v -> Map k v -> Map k v             O(1)
            lookupM :: Eq k => k -> Map k v -> Maybe v                 O(n)
            deleteM :: Eq k => k -> Map k v -> Map k v                 O(n)
            keys :: Map k v -> [k]                                     O(1)
-}

emptyM :: Map k v
--Propósito: devuelve un map vacío.
emptyM = M [] []

assocM :: Eq k => k -> v -> Map k v -> Map k v
-- Propósito: agrega una asociación clave-valor al map.
assocM k v (M ks vs) = M (k:ks) (v:vs) 

lookupM :: Eq k => k -> Map k v -> Maybe v
-- Propósito: encuentra un valor dado una clave.
lookupM k (M ks vs) = valorEnPosicion (posicionDeClave k ks) vs

valorEnPosicion :: Int -> [v] -> Maybe v
valorEnPosicion _ []     = Nothing
valorEnPosicion 0 (v:vs) = Just v
valorEnPosicion x (v:vs) = valorEnPosicion (x - 1) vs

posicionDeClave :: k -> [k] -> Int
posicionDeClave _  []     = 0
posicionDeClave k1 (k:ks) = if k == k1
                                then 0
                                else 1 + posicionDeClave

deleteM :: Eq k => k -> Map k v -> Map k v
-- Propósito: borra una asociación dada una clave.
deleteM k (M ks vs) = M (borrarPosicion (posicionDeClave k ks) ks) (borrarPosicion (posicionDeClave k ks) vs)

borrarPosicion :: Int -> [a] -> [a]
borrarPosicion _ []     = []
borrarPosicion 0 (x:xs) = x
borrarPosicion x (x:xs) = borrarPosicion (x - 1) xs

keys :: Map k v -> [k]
-- Propósito: devuelve las claves del map.
keys (M ks vs) = ks