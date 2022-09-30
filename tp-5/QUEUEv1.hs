module QUEUEv1
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
--FIFO (firrst in, first out)
--Los elementos deben encolarse por el final de la lista y desencolarse por delante.
{-
Opereciones:                                    costos:
            emptyQ :: Queue a                          O(1)
            isEmptyQ :: Queue a -> Bool                O(1)
            enqueue :: a -> Queue a -> Queue a         O(n)
            firstQ :: Queue a -> a                     O(1)
            dequeue :: Queue a -> Queue a              O(1)
-}

-- 1 : 2 : 3 : 5 : 6 : []
--El primero que entro es 1 y el último es 6. Tomando como referencia eso realizar lo siguiente:

emptyQ :: Queue a
-- Crea una cola vacía.
emptyQ = Q []

isEmptyQ :: Queue a -> Bool
-- Dada una cola indica si la cola está vacía.
isEmptyQ (Q xs) = null xs

enqueue :: a -> Queue a -> Queue a
-- Dados un elemento y una cola, agrega ese elemento a la cola.
enqueue x (Q xs) = Q (agregarAlFinal x xs)

agregarAlFinal :: a -> [a] ->[a]
agregarAlFinal x1 []     = x1:[]
agregarAlFinal x1 (x:xs) = x : agregarAlFinal x1 xs

firstQ :: Queue a -> a
-- Dada una cola devuelve el primer elemento de la cola.
firstQ (Q xs) = head xs

dequeue :: Queue a -> Queue a
-- Dada una cola la devuelve sin su primer elemento.
dequeue (Q xs) = Q (tail xs)