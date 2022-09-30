module QUEUEv2
    (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
where
    
data Queue a = Q [a] deriving Show
{-
    INVARIANTE DE REPRESENTACIÓN: En Q []
        -
        -

    CASOS VALIDOS:
        -
    CASOS INVALIDOS:
        -
-}
--FIFO (first in, first out)
--Los elementos deben encolarse por adelante de la lista y desencolarse por el final.
{-
Opereciones:                                    costos:
            emptyQ :: Queue a                          O(1)
            isEmptyQ :: Queue a -> Bool                O(1)
            enqueue :: a -> Queue a -> Queue a         O(1)
            firstQ :: Queue a -> a                     O(n)
            dequeue :: Queue a -> Queue a              O(n)
-}

-- 1 : 2 : 3 : 5 : 6 : []
--El primero que entro es 6 y el último es 1. Tomando como referencia eso realizar lo siguiente:

emptyQ :: Queue a
-- Crea una cola vacía.
emptyQ = Q []

isEmptyQ :: Queue a -> Bool
-- Dada una cola indica si la cola está vacía.
isEmptyQ (Q xs) = null xs

enqueue :: a -> Queue a -> Queue a
-- Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue x (Q xs) = Q (x:xs)

firstQ :: Queue a -> a
-- Dada una cola devuelve el primer elemento de la cola.
firstQ (Q xs) = ultimoElemento xs

ultimoElemento :: [a] -> a
--PRECONDICIÓN: La lista no debe ser vacia.
ultimoElemento (x:[]) = x
ultimoElemento (_:xs) = ultimoElemento xs

dequeue :: Queue a -> Queue a
-- Dada una cola la devuelve sin su primer elemento.
dequeue (Q xs) = Q (sinElUltimo xs)

sinElUltimo :: [a] -> [a]
--PRECONDICIÓN: La lista no debe ser vacia.
sinElUltimo (_:[]) = []
sinElUltimo (x:xs) = x : sinElUltimo xs