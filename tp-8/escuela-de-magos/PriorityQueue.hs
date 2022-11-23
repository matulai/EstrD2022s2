module PriorityQueue(
  PriorityQueue,
  emptyPQ,
  isEmptyPQ,
  insertPQ,
  maxPQ,
  deleteMaxPQ
)

where 

data PriorityQueue a = PQ [a]

-- Propósito: devuelve una priority queue vacía.
emptyPQ :: PriorityQueue a
emptyPQ = PQ []

-- Propósito: indica si la priority queue está vacía.
isEmptyPQ :: PriorityQueue a -> Bool
isEmptyPQ (PQ xs) = null xs

-- Propósito: inserta un elemento en la priority queue.
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
insertPQ x (PQ ys) = PQ (ordenarEn x ys)

ordenarEn x [] = [x]
ordenarEn x (y:ys) = if x > y then x:(y:ys) else y: (ordenarEn x ys)

-- Propósito: devuelve el elemento más prioriotario (el mínimo) 
-- de la priority queue.
-- Precondición: parcial en caso de priority queue vacía.
maxPQ :: Ord a => PriorityQueue a -> a
maxPQ (PQ ys) = head ys

-- Propósito: devuelve una priority queue sin el elemento 
-- más prioritario (el mínimo).
-- Precondición: parcial en caso de priority queue vacía.
deleteMaxPQ :: Ord a => PriorityQueue a -> PriorityQueue a
deleteMaxPQ (PQ ys) = PQ $ tail ys

intercalar :: (Show a) => [a] -> String
intercalar [] = ""
intercalar [x] = show x
intercalar (x:xs) = (show x) ++ " > " ++ (intercalar xs)

instance Show a => Show (PriorityQueue a) where 
  show (PQ xs) = "[" ++ (intercalar xs) ++ "]"