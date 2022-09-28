module SetSinRepetidos
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
where

data Set a = S [a] Int deriving Show
--data Set a = S ([a], Int)
{-
    INVARIANTE DE REPRESENTACIÓN: En S xs y
        -La lista "xs" no tiene repetidos.
        -
    CASOS VALIDOS:
        - S [1,2,3,4]
    CASOS INVALIDOS:
        - S [1,2,3,4,1,2,3]
-}
emptyS :: Set a
-- Crea un conjunto vacío.
emptyS = S [] 0

addS :: Eq a => a -> Set a -> Set a
-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS x (S xs n) = if elem x xs
                        then S xs n
                        else S (x:xs) (n + 1)

belongs :: Eq a => a -> Set a -> Bool
--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs x (S xs _) = elem x xs

sizeS :: Eq a => Set a -> Int
--Devuelve la cantidad de elementos distintos de un conjunto.
sizeS (S xs n) = n

removeS :: Eq a => a -> Set a -> Set a
--Borra un elemento del conjunto.
removeS x (S xs n) = S (sacar x xs) (n - 1)

sacar :: Eq a => a -> [a] -> [a]
sacar _ []     = []
sacar x1 (x:xs) = if x1 == x
                    then xs
                    else x1 : sacar x1 xs

unionS :: Eq a => Set a -> Set a -> Set a
-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
unionS (S xs n) (S ys n1) = S (xs ++ ys) (n + n1)  

setToList :: Eq a => Set a -> [a]
--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList (S xs n) = xs 