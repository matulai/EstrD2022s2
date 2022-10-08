module SETSinRepetidos
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

{-
emptyS :: Set a                             O(1)                   
addS :: Eq a => a -> Set a -> Set a         O(n)
belongs :: Eq a => a -> Set a -> Bool       O(n)
sizeS :: Eq a => Set a -> Int               O(1)
removeS :: Eq a => a -> Set a -> Set a      O(n)
unionS :: Eq a => Set a -> Set a -> Set a   O(n^2)
setToList :: Eq a => Set a -> [a]           O(1)
-}
emptyS :: Set a                                                                                     
-- Crea un conjunto vacío.
emptyS = S [] 0

addS :: Eq a => a -> Set a -> Set a
-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS x (S xs n) = if elem x xs
                    then S xs n
                    else S (x:xs) (n+1)

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
unionS (S xs n) (S ys n1) = S (juntarSinRepetir xs ys) (n + n1)  

juntarSinRepetir :: Eq a => [a] -> [a] -> [a]
juntarSinRepetir []     ys = []
juntarSinRepetir (x:xs) ys = agregar x (juntarSinRepetir xs ys)

agregar :: Eq a => a -> [a] -> [a]
agregar _ []     = []
agregar x (y:ys) = if x == y
                        then x : ys
                        else y : agregar x ys

setToList :: Eq a => Set a -> [a]
--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList (S xs n) = xs 