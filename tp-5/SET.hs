module SET
    (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
where

data Set a = S [a] Int
{-
    INVARIANTE DE REPRESENTACIÓN: En S xs y
        -La lista "xs" no tiene repetidos.
        -

    CASOS VALIDOS:
        -
    CASOS INVALIDOS:
        -
-}

emptyS :: Set a
emptyS = S [] 0

addS :: Eq a => a -> Set a -> Set a
addS x (S xs n) = if pertenece x xs
                        then S xs n
                        else S (x:xs) (n + 1)

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ []       = False
pertenece x (y : ys) = x == y || pertenece x ys

belongs :: Eq a => a -> Set a -> Bool
belongs x (S xs _) = pertenece x xs

sizeS :: Eq a => Set a -> Int
sizeS (S xs n) = n

removeS :: Eq a => a -> Set a -> Set a
removeS x (S ys n) = if pertenece x ys 
                        then S (sacar x ys) (n - 1)
                        else S ys n

sacar :: Eq a => a -> [a] -> [a]
sacar _ []     = []
sacar x (y:ys) = if x == y
                    then ys
                    else y : sacar x ys

unionS :: Eq a => Set a -> Set a -> Set a
unionS = undefined

setToList :: Eq a => Set a -> [a]
setToList = undefined