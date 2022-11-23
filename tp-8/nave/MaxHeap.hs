module MaxHeap(
  MaxHeap,
  emptyH,
  isEmptyH,
  insertH,
  maxH,
  deleteMaxH
)

where 

data MaxHeap a = H [a]

-- Propósito: devuelve una priority queue vacía.
emptyH :: MaxHeap a
emptyH = H []

-- Propósito: indica si la priority queue está vacía.
isEmptyH :: MaxHeap a -> Bool
isEmptyH (H xs) = null xs

-- Propósito: inserta un elemento en la priority queue.
insertH :: Ord a => a -> MaxHeap a -> MaxHeap a
insertH x (H ys) = H (ordenarEn x ys)

ordenarEn x [] = [x]
ordenarEn x (y:ys) = if x > y then x:(y:ys) else y: (ordenarEn x ys)

-- Propósito: devuelve el elemento más prioriotario (el mínimo) 
-- de la priority queue.
-- Precondición: parcial en caso de priority queue vacía.
maxH :: Ord a => MaxHeap a -> a
maxH (H ys) = head ys

-- Propósito: devuelve una priority queue sin el elemento 
-- más prioritario (el mínimo).
-- Precondición: parcial en caso de priority queue vacía.
deleteMaxH :: Ord a => MaxHeap a -> MaxHeap a
deleteMaxH (H ys) = H $ tail ys

intercalar :: (Show a) => [a] -> String
intercalar [] = ""
intercalar [x] = show x
intercalar (x:xs) = (show x) ++ " > " ++ (intercalar xs)

instance Show a => Show (MaxHeap a) where 
  show (H xs) = "[" ++ (intercalar xs) ++ "]"