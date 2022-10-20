{-
Modo: Implementador
Representación: 
    data RAList a = MkR Int (Map Int a) (Heap a)

Invariante de Representación:
    -Si el elemento existe en el Map Int-elemento entonces existe en el Heap elemento y viceversa
    -El número que representa la siguiente posición también nos indica la cantidad de claves del 
     Map y elementos del Heap, ademas si el numero de la siguiente posición es cero entonces la lista
     es vacia.
    -Los valores de las claves del Map no pueden ser Menor a cero o Mayor o igual al número que representa
     la siguiente posición.
    -
    -
    -
-}

emptyRAL :: RAList a
--Propósito: devuelve una lista vacía.
--Eficiencia: O(1).
emptyRAL = MkR 0 emptyM emptyH

isEmptyRAL :: RAList a -> Bool
--Propósito: indica si la lista está vacía.
--Eficiencia: O(1).
isEmptyRAL (MkR x _ _) = x == 0  

lengthRAL :: RAList a -> Int
--Propósito: devuelve la cantidad de elementos.
--Eficiencia: O(1).
lengthRAL (MkR x _ _) = x 

get :: Int -> RAList a -> a
--Propósito: devuelve el elemento en el índice dado.
--Precondición: el índice debe existir.
--Eficiencia: O(log N).
get x (MkR _ mIa _) = fromJust (lookupM x mIa)

minRAL :: Ord a => RAList a -> a
--Propósito: devuelve el mínimo elemento de la lista.
--Precondición: la lista no está vacía.
--Eficiencia: O(1).
minRAL (MkR _ _ ha) = findMin ha 

add :: Ord a => a -> RAList a -> RAList a
--Propósito: agrega un elemento al final de la lista.
--Eficiencia: O(log N).
add e (MkR x mIa ha) = MkR (x+1) (assocM x e mIa) (insertH e ha)

elems :: Ord a => RAList a -> [a]
--Propósito: transforma una RAList en una lista, respetando el orden de los elementos.
--Eficiencia: O(N log N).
elems (MkR x mIa _) = raToList 0 x mIa

raToList :: Ord a => Int -> Int -> Map Int a -> [a]
--Eficiencia: O(K log K) siendo K la cantidad de claves de la lista.
raToList x y mIa = if x == y 
                        then []
                        else fromJust (lookupM mIa x) : raToList (x + 1) y mIa

remove :: Ord a => RAList a -> RAList a
--Propósito: elimina el último elemento de la lista.
--Precondición: la lista no está vacía.
--Eficiencia: O(N log N).
remove (MkR x mIa ha) = let e = fromJust (lookupM (x-1) mIa) 
                        MkR (x - 1) (deleteM (x-1) mIa) (removerH e ha)

removerH :: Ord a => a -> Heap a -> Heap a
--Eficiencia: O(N log N)
removerH e ha = if e == findMin ha
                    then ha
                    else insertH (findMin ha) (removerH e (deleteMin ha))

set :: Ord a => Int -> a -> RAList a -> RAList a
--Propósito: reemplaza el elemento en la posición dada.
--Precondición: el índice debe existir.
--Eficiencia: O(N log N).
set x e (MkR y mIa ha) = let e1 = fromJust (lookupM x mIa) in
                         MkR y (assocM x e mIa) (insertH e (removerH e1 ha))    


addAt :: Ord a => Int -> a -> RAList a -> RAList a
--Propósito: agrega un elemento en la posición dada.
--Precondición: el índice debe estar entre 0 y la longitud de la lista.
--Observación: cada elemento en una posición posterior a la dada pasa a estar en su posición siguiente.
--Eficiencia: O(N log N).
--Sugerencia: definir una subtarea que corra los elementos del Map en una posición a partir de una posición dada. Pasar
--también como argumento la máxima posición posible.
addAt x e (MkR y mIa ha) = MkR (y + 1) (assoc x e (desplazarM x y mIa)) (insertH e ha)

desplazarM :: Ord a => Int -> Int -> Map Int a -> Map Int a
--Eficiencia: O(K log K) siendo K la cantidad de claves del Map. 
desplazarM x y mIa = if (x >= y)
                        then mIa 
                        else let e = fromJust (lookupM mIa x) in
                             assoc (x+1) e (desplazarM (x+1) y mIa)