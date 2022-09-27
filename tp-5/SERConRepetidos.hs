module SetConRepetidos
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
where

data Set a = S [a] Int deriving Show

emptyS :: Set a
-- Crea un conjunto vacío.
emptyS = S [] 0

addS :: Eq a => a -> Set a -> Set a
-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS x (S xs n) = S (x:xs) n

belongs :: Eq a => a -> Set a -> Bool
-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs x (S xs _) = elem x xs 

sizeS :: Eq a => Set a -> Int
-- Devuelve la cantidad de elementos distintos de un conjunto.
sizeS (S xs n) = n

removeS :: Eq a => a -> Set a -> Set a
-- Borra un elemento del conjunto.
removeS x (S xs n) = if elem x xs 
                        then S (sacarTodosLos x xs) (n - 1)
                        else S xs n

sacarTodosLos :: Eq a => a -> [a] -> [a]
sacarTodosLos _ []      = []
sacarTodosLos x1 (x:xs) = if x1 == x
                            then sacarTodosLos xs
                            else x1 : sacarTodosLos x1 xs

unionS :: Eq a => Set a -> Set a -> Set a
-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
unionS (S xs n) (S ys n1) = S (xs ++ ys) (n + n1) 

setToList :: Eq a => Set a -> [a]
-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList (S xs n) = xs  
