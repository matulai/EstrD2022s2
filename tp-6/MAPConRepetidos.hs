module MAPConRepetidos
    (Map, emptyM, assocM, lookupM, deleteM, keys)
where

data Map k v = M [(k,v)]
 
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
Opereciones:                                                    costos:
            emptyM :: Map k v                                          O(1)
            assocM :: Eq k => k -> v -> Map k v -> Map k v             O(1)
            lookupM :: Eq k => k -> Map k v -> Maybe v                 O(n^2)
            deleteM :: Eq k => k -> Map k v -> Map k v                 O(n)
            keys :: Map k v -> [k]                                     O(n^2)
-}

emptyM :: Map k v
--Propósito: devuelve un map vacío
emptyM = M []

assocM :: Eq k => k -> v -> Map k v -> Map k v
--Propósito: agrega una asociación clave-valor al map.
assocM k v (M kvs) = M ((k,v):kvs)

lookupM :: Eq k => k -> Map k v -> Maybe v
--Propósito: encuentra un valor dado una clave.
lookupM k (M kvs) = valorDeClave k kvs

valorDeClave :: Eq k => k -> [(k,v)] -> Maybe v
valorDeClave _  []          = Nothing
valorDeClave k1 ((k,v):kvs) = if k1 == k
                                    then Just v
                                    else valorDeClave k1 kvs

deleteM :: Eq k => k -> Map k v -> Map k v
--Propósito: borra una asociación dada una clave.
deleteM k (M kvs) = M (borrarM k kvs)

borrarM :: Eq k => k -> [(k,v)] -> [(k,v)]
borrarM _  []          = []
borrarM k1 ((k,v):kvs) = if k1 == k 
                            then borrarM k1 kvs
                            else (k,v) : borrarM k1 kvs

keys :: Map k v -> [k]
--Propósito: devuelve las claves del map.
keys (M kvs) = clavesM kvs

clavesM :: [(k,v)] -> [k]
clavesM []          = []
clavesM ((k,v):kvs) = if existeClaveEn k kvs
                            then clavesM kvs
                            else k : clavesM kvs

existeClaveEn :: Eq k => k -> [(k,v)] -> Bool
existeClaveEn _  []          = False
existeClaveEn k1 ((k,v):kvs) = k1 == k || existeClaveEn k1 kvs