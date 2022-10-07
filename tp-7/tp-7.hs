{-	
------------------------------------------------------
||	PRÁCTICA N°7 || HEAPS Y BSTs                    ||
||	Alumno: Matias Laime					        ||
||	Fecha De Inicio: 06/10/2022				        ||
------------------------------------------------------
-}
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show


arbol = NodeT 50 (NodeT 45 (NodeT 40 EmptyT EmptyT) (NodeT 47 EmptyT EmptyT)) (NodeT 55 (NodeT 53 EmptyT EmptyT) (NodeT 57 EmptyT EmptyT))

--Ejercicio 2

belongsBST :: Ord a => a -> Tree a -> Bool
--Propósito: dado un BST dice si el elemento pertenece o no al árbol.
--Costo: O(log N)
belongsBST _ EmptyT           = False
belongsBST x (NodeT x1 t1 t2) = if x < x1
                                    then belongsBST x t1
                                    else if x > x1
                                        then belongsBST x t2
                                        else x == x1 

insertBST :: Ord a => a -> Tree a -> Tree a
-- Propósito: dado un BST inserta un elemento en el árbol.
-- Costo: O(log N)
insertBST x EmptyT           = NodeT x EmptyT EmptyT
insertBST x (NodeT x1 t1 t2) = if x < x1
                                    then NodeT x1 (insertBST x t1) t2
                                    else NodeT x1 t1 (insertBST x t2)            

deleteBST :: Ord a => a -> Tree a -> Tree a
-- Propósito: dado un BST borra un elemento en el árbol.
-- Costo: O(log N)                                
deleteBST x EmptyT           = EmptyT
deleteBST x (NodeT x1 t1 t2) = if x < x1 
                                    then NodeT x1 (deleteBST x t1) t2
                                    else if x > x1
                                        then NodeT x1 t1 (deleteBST x t2)
                                        else armarBST t1 t2 

armarBST :: Ord a => Tree a -> Tree a -> Tree a
armarBST EmptyT t2 = t2
armarBST t1     t2 = NodeT (maxEnArbol t2) t1 (delArbolMax t2) 

maxEnArbol :: Ord a => Tree a -> a
maxEnArbol (NodeT x EmptyT EmptyT) = x
maxEnArbol (NodeT x t1 t2)         = maxEnArbol t2

delArbolMax :: Ord a => Tree a -> Tree a
delArbolMax (NodeT _ EmptyT EmptyT) = EmptyT
delArbolMax (NodeT x t1 t2)         = NodeT x t1 (delArbolMax t2)