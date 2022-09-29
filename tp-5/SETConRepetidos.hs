module SetConRepetidos
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
where

data Set a = S [a] Int deriving Show
{-
    INVARIANTE DE REPRESENTACIÓN: En S xs y
        -
        -
    CASOS VALIDOS:
        -
    CASOS INVALIDOS:
        -
-}

{-
emptyS :: Set a                             O(1)                   
addS :: Eq a => a -> Set a -> Set a         O(1)
belongs :: Eq a => a -> Set a -> Bool       O(n)
sizeS :: Eq a => Set a -> Int               O(n^2)
removeS :: Eq a => a -> Set a -> Set a      O(n)
unionS :: Eq a => Set a -> Set a -> Set a   O(n)
setToList :: Eq a => Set a -> [a]           O(n^2)
-}

emptyS :: Set a
-- Crea un conjunto vacío.
emptyS = S [] 0

addS :: Eq a => a -> Set a -> Set a
-- Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS x (S xs n) = S (x:xs) (n + 1)

belongs :: Eq a => a -> Set a -> Bool
-- Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs x (S xs _) = elem x xs 

sizeS :: Eq a => Set a -> Int
-- Devuelve la cantidad de elementos distintos de un conjunto.
sizeS (S xs _) = cantidadDistintosEn xs

cantidadDistintosEn :: Eq a => [a] -> Int
cantidadDistintosEn []     = 0
cantidadDistintosEn (x:xs) = unoSi (elem x xs) + cantidadDistintosEn xs

unoSi :: Bool -> Int
unoSi True  = 1
unoSi _     = 0

removeS :: Eq a => a -> Set a -> Set a
-- Borra un elemento del conjunto.
removeS x (S xs n) = (S (sacar x xs) n) 

sacar :: Eq a => a -> [a] -> [a]
sacar _ []      = []
sacar x1 (x:xs) = if x == x1
                        then sacar x1 xs
                        else x : sacar x1 xs

unionS :: Eq a => Set a -> Set a -> Set a
-- Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
unionS (S xs n) (S ys n1) = S (xs ++ ys) (n + n1) 

setToList :: Eq a => Set a -> [a]
-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.
setToList (S xs n) = listaSinRepetidos xs

listaSinRepetidos :: Eq a => [a] -> [a]
listaSinRepetidos []     = []
listaSinRepetidos (x:xs) = if elem x xs
                                then listaSinRepetidos xs
                                else x : listaSinRepetidos xs