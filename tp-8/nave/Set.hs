module Set(
  Set,
  emptyS,
  addS,
  belongs,
  sizeS,
  removeS,
  unionS,
  setToList
)

where

data Set a = S [a]

{- INVARIANTES:
  - la lista no puede tener elementos repetidos
-}

-- Crea un conjunto vacío.
emptyS :: Set a
emptyS = S []


-- Dados un elemento y un conjunto, agrega el 
-- elemento al conjunto.
addS :: Eq a => a -> Set a -> Set a
addS x (S xs) = S $ if elem x xs then xs else x:xs 

-- Dados un elemento y un conjunto indica si el 
-- elemento pertenece al conjunto.
belongs :: Eq a => a -> Set a -> Bool
belongs x (S xs) = elem x xs


-- Devuelve la cantidad de elementos distintos 
-- de un conjunto.
sizeS :: Eq a => Set a -> Int
sizeS (S xs) = length xs


-- Borra un elemento del conjunto.
removeS :: Eq a => a -> Set a -> Set a
removeS x (S xs) = S (removeS' x xs)

removeS' _ [] = []
removeS' x (y:ys) = if x == y then ys else y:(removeS' x ys)

-- Dados dos conjuntos devuelve un conjunto con 
-- todos los elementos de ambos. conjuntos.
unionS :: Eq a => Set a -> Set a -> Set a
unionS (S xs) (S ys) = S $ unionS' xs ys

unionS' [] ys = ys
unionS' (x:xs) ys = (if elem x ys then [] else [x]) ++ unionS' xs ys

-- Dado un conjunto devuelve una lista con todos 
-- los elementos distintos del conjunto
setToList :: Eq a => Set a -> [a]
setToList = undefined

intercalar :: (Show a) => [a] -> String
intercalar [] = ""
intercalar [x] = show x
intercalar (x:xs) = (show x) ++ "," ++ (intercalar xs)


instance (Show a) => Show (Set a) where 
  show (S xs) = "{" ++ (intercalar xs) ++ "}"