module Map(
  Map,
  emptyM,
  assocM,
  lookupM,
  deleteM,
  keys
)
where

data Map k v = M [(k,v)]

-- Propósito: devuelve un map vacío
emptyM :: Map k v
emptyM = M []

-- Propósito: agrega una asociación clave-valor al map.
assocM :: Eq k => k -> v -> Map k v -> Map k v
assocM k v (M kvs) = M $ assocM' k v kvs

assocM' k v [] = [(k,v)]
assocM' k v (kv:kvs) = 
  if fst kv == k
    then (k,v):kvs
    else kv:(assocM' k v kvs)

-- Propósito: encuentra un valor dado una clave.
lookupM :: Eq k => k -> Map k v -> Maybe v
lookupM k (M kvs) = lookupM' k kvs

lookupM' k [] = Nothing
lookupM' k (kv:kvs) = 
  if fst kv == k
    then Just $ snd kv
    else lookupM' k kvs

-- Propósito: borra una asociación dada una clave.
deleteM :: Eq k => k -> Map k v -> Map k v
deleteM k (M kvs) = M $ deleteM' k kvs

deleteM' k [] = []
deleteM' k (kv:kvs) = 
  if fst kv == k
    then kvs
    else kv : (deleteM' k kvs)

-- Propósito: devuelve las claves del map.
keys :: Map k v -> [k]
keys (M kvs) = map fst kvs




intercalar :: (Show k, Show v) => [(k,v)] -> String
intercalar [] = ""
intercalar [(k,v)] = "\n  " ++ show k ++ " -> " ++ show v ++ "\n"
intercalar ((k,v):kvs) = "\n  " ++ show k ++ " -> " ++ show v ++ "," ++ (intercalar kvs)

instance (Show k, Show v) => Show (Map k v) where 
  show (M xs) = "{" ++ (intercalar xs) ++ "}"