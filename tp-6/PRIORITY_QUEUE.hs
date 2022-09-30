module PRIORITY_QUEUE
    (PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)
where 

data PriorityQueue a = PQ [a] 
{-
    INVARIANTE DE REPRESENTACIÓN: En S []
        -
        -
    CASOS VALIDOS:
        -
    CASOS INVALIDOS:
        -
-}
{-
Opereciones:                                    costos:
            emptyQ :: Queue a                                                               O(1)
            isEmptyQ :: Queue a -> Bool                                                     O(1)
            insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a                    O(1)
            findMinPQ :: Ord a => PriorityQueue a -> a                                      O(n)
            deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a                      O(n)
-}

emptyPQ :: PriorityQueue a
--Propósito: devuelve una priority queue vacía.
emptyPQ = PQ []

isEmptyPQ :: PriorityQueue a -> Bool
--Propósito: indica si la priority queue está vacía.
isEmptyPQ (PQ xs) = null xs

insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
--Propósito: inserta un elemento en la priority queue.
insertPQ x (PQ xs) = PQ (x:xs)

findMinPQ :: Ord a => PriorityQueue a -> a
--Propósito: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
--Precondición: parcial en caso de priority queue vacía.
findMinPQ (PQ xs) = minimum xs

deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a
--Propósito: devuelve una priority queue sin el elemento más prioritario (el mínimo).
--Precondición: parcial en caso de priority queue vacía.
deleteMinPQ (PQ xs) = PQ (borrar (minium xs) xs)

borrar ::  