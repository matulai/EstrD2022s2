module QUEUEv3
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
where

data Queue a = Q [a] [a] deriving Show

{-
    INVARIANTE DE REPRESENTACIÓN: En Q fs bs
        -si "fs" se encuentra vacia entonces bs tambien esta vacia.
        -

    CASOS VALIDOS:
        -
    CASOS INVALIDOS:
        -
-}

emptyQ :: Queue a
--Crea una cola vacía.
emptyQ = Q [] []

isEmptyQ :: Queue a -> Bool
--Dada una cola indica si la cola está vacía.
isEmptyQ (Q [] _) = True

enqueue :: a -> Queue a -> Queue a
--Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue x (Q ys zs) = Q ((head zs):ys) (x:[])

firstQ :: Queue a -> a
--Dada una cola devuelve el primer elemento de la cola.


dequeue :: Queue a -> Queue a
--Dada una cola la devuelve sin su primer elemento.
