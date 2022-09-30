module QUEUEv3
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
where

data Queue a = Q [a] [a] deriving Show
--               fs  bs
--fs: Front Stack
--bs: Back Stack
{-
    INVARIANTE DE REPRESENTACIÓN: En Q fs bs
        -si "fs" se encuentra vacia entonces bs tambien esta vacia.
        -
    CASOS VALIDOS:
        -
    CASOS INVALIDOS:
        -
-}
{-
Opereciones:                                    costos:
            emptyQ :: Queue a                          O(1)
            isEmptyQ :: Queue a -> Bool                O(1)
            enqueue :: a -> Queue a -> Queue a         O(1)
            firstQ :: Queue a -> a                     O(1)
            dequeue :: Queue a -> Queue a              O(n^2)
-}

emptyQ :: Queue a
--Crea una cola vacía.
emptyQ = Q [] []

isEmptyQ :: Queue a -> Bool
--Dada una cola indica si la cola está vacía.
isEmptyQ (Q xs _) = null xs

enqueue :: a -> Queue a -> Queue a
--Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue x (Q ys zs) = if null ys
                            then Q (x:ys) zs
                            else Q ys (x:zs)

firstQ :: Queue a -> a
--Dada una cola devuelve el primer elemento de la cola.
firstQ (Q ys zs) = elPrimeroDe ys  

elPrimeroDe :: [a] -> [a]
elPrimeroDe []     = []
elPrimeroDe (x:xs) = x

dequeue :: Queue a -> Queue a
--Dada una cola la devuelve sin su primer elemento.
dequeue (Q ys zs) = Q (inverso zs) []

inverso :: [a] -> [a]
inverso []     = []
inverso (x:xs) = inverso ++ [x]