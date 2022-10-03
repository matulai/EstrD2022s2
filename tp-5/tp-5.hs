-- import SetSinRepetidos
import SetConRepetidos
-- import QUEUEv1
import QUEUEv2
--import STACK

{-	
------------------------------------------------------
||	PRÁCTICA N°5 ||SET, STACK y QUEUE             	||
||	Alumno: Matias Laime					        ||
||	Fecha De Inicio: 25/09/2022				        ||
------------------------------------------------------
-}
--1.|CÁLCULO DE COSTOS|

--Los costos pueden ser: |CONSTANTE, LINEAL y CUADRATICA| por ahora...
--COSTO CONSTANTE: La funcion siempre tarda lo mismo, no depende de la cantidad de elementos de la estructura.
--COSTO LINEAL: Por cada elemento de la estructura, solamente se hacen operaciones de costo CONSTANTE.
--COSTO CUADRATICO: Por cada elemento de la estructura, solamente se hacen operaciones de costo LINEAL.
--NUNCA olvidarse de poner los costos y el invariante de representacion.
------------------------------------------------------
--Costo: Constante O(1)
head' :: [a] -> a
head' (x:xs) = x

--Costo: Constante O(1)
sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

--Costo: Lineal O(n). Depende del valor dado. 
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

--Costo: Lineal O(n). Depende de la cantidad de elementos de la lista dada.
longitud :: [a] -> Int
longitud []     = 0
longitud (x:xs) = 1 + longitud xs

--Costo: Cuadratica O(N^2). Invoca a la funcion "factorial"(con un costo lineal) por cada elemento de la lista dada.
--El peor caso posible es que todos los numeros de la lista sean iguales.
--Es costo depende de los valores de la lista de numeros, no de la longitud de la misma.
factoriales :: [Int] -> [Int]
factoriales []     = []
factoriales (x:xs) = factorial x : factoriales xs

--Costo: Lineal O(1). Solo se hacen operaciones de costo constantes por cada elemento.
--Por cada elemento de la lista se hacen dos operaciones de costo constantes (con operador de comparacion y operador logico).
pertenece :: Eq a => a -> [a] -> Bool
pertenece n []     = False
pertenece n (x:xs) = n == x || pertenece n xs

--Costo: Cuadratica O(n^2). Invoca a la funcion "pertenece" por cada elemento de la lista dada.
-- por cada elemento de la lista de elementos se ejecuta una funcion de costo lineal.
-- sinRepetidos :: Eq a => [a] -> [a]
-- sinRepetidos []     = []
-- sinRepetidos (x:xs) =   if pertenece x xs
--                             then sinRepetidos xs
--                             else x : sinRepetidos xs

--Costo: Lineal O(n). Depende de la cantidad de elementos de la primera lista.
--Se realiza una operacion constante (cons) por cada elemento de la lista.
-- equivalente a (++)
append :: [a] -> [a] -> [a]
append [] ys     = ys
append (x:xs) ys = x : append xs ys

--Costo: Cuadratica(n^2). Depende de la cantidad de elementos de la lista de string.
--Por cada elemento de la lista se ejecuta una operacion lineal (append ++ o agregar).
concatenar :: [String] -> String
concatenar []     = []
concatenar (x:xs) = x ++ concatenar xs

--Costo: Lineal O(n). Tomando el peor caso posible que el tamaño de la lista sea mayor o igual a n. 
--                    Depende del numero dado osino de la cantidad de elementos si la cantidad es menor a n.
--Por cada recorrido se ejecuta una operacion constante.
takeN :: Int -> [a] -> [a]
takeN 0 xs     = []
takeN n []     = []
takeN n (x:xs) = x : takeN (n-1) xs

--Costo: Lineal O(n). Peor caso posible que el tamaño de la lista sea mayor o igual a n. 
-- Igual que la funcion takeN. 
dropN :: Int -> [a] -> [a]
dropN 0 xs     = xs
dropN n []     = []
dropN n (x:xs) = dropN (n-1) xs

--Costo: Lineal O(n)
--No se realiza un recorrdio, se ejecutan otras funciones lineales lo cual lo hace una funcion de costo lineal. 
partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)

--Costo: Cuadratico O(n^2)
--se realiza un recorrido en el cual por cada uno se ejecuta una funcion lineal.
minimo :: Ord a => [a] -> a
minimo [x]    = x
minimo (x:xs) = min x (minimo xs)

--Costo: Lineal O(n)
--La funcion depende de la cantidad de elementos de la lista de elementos.
sacar :: Eq a => a -> [a] -> [a]
sacar n []     = []
sacar n (x:xs) =    if n == x
                        then xs
                        else x : sacar n xs

--Costo: Cuadratico O(n^2)
--Se ejecutan dos funciones lineales por cada elemento de la lista de elementos lo que le da un costo cuadratico. 
ordenar :: Ord a => [a] -> [a]
ordenar [] = []
orderar xs =    let m = minimo xs
                    in m : ordenar (sacar m xs)

--2|SET (CONJUNTO)|

--2.
data Tree a = EmptyS | NodeT a (Tree a) (Tree a)
------------------------------------------------------------------
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
--Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecenal conjunto.
losQuePertenecen xs sa = elementosDeQuePertenecenA xs (setToList sa)

elementosDeQuePertenecenA :: Eq a => [a] -> [a] -> [a]
elementosDeQuePertenecenA []     _ = []
elementosDeQuePertenecenA (x:xs) ys = if elem x ys
                                            then x : elementosDeQuePertenecenA xs ys
                                            else elementosDeQuePertenecenA xs ys 

sinRepetidos :: Eq a => [a] -> [a]
--Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
sinRepetidos []     = []
sinRepetidos (x:xs) = if elem x xs
                            then sinRepetidos xs 
                            else x : sinRepetidos xs

unirTodos :: Eq a => Tree (Set a) -> Set a
--Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos del arbol.
unirTodos EmptyS           = emptyS
unirTodos (NodeT sa t1 t2) = unionS sa (unionS (unirTodos t1) (unirTodos t2))

{-
3.La diferencia recae en los costos de operacion en ambas, en la cual la variante "SETSinRepetidos"
  tiene un costo menor que la variante "SETConRepetidos". La primer variante mencionada soluciona el
  problema de repetidos al agregar un elemento al conjunto elevando un poco el costo en esa funcion mientras
  que la segunda variante mencionada admite que se agregen repetidos al conjunto disminuyendo el costo
  de esa funcion y soluciona el problema de repetidos cuando se llaman a las demas funciones lo cual hara
  que esas funciones eleven su costo para solucionar ese problema. 
-}

--3|QUEUE|

--2.
lengthQ :: Queue a -> Int
--Cuenta la cantidad de elementos de la cola.
lengthQ q = if isEmptyQ q
                then 0
                else 1 + lengthQ (dequeue q)

queueToList :: Queue a -> [a]
--Dada una cola devuelve la lista con los mismos elementos, donde el orden de la lista es el de la cola.
--Nota: chequear que los elementos queden en el orden correcto.
queueToList q = if isEmptyQ q
                    then []
                    else firstQ q : queueToList (dequeue q)

unionQ :: Queue a -> Queue a -> Queue a
--Inserta todos los elementos de la segunda cola en la primera.
unionQ q1 q2 = if isEmptyQ q1
                    then q2
                    else unionQ (dequeue q1) (enqueue (firstQ q1) q2)
{-
3.La diferencia entre las variantes "QUEUEv1" y "QUEUEv2" recae en el costo de operacion. Como sabemos la
  las listas se escriben de la siguiente manera:
                    1 : 2 : 3 []  (una lista de números)
  Con esto sabido, podemos deducir cual variante tendra mas costo. En primera varitante mencionada se 
  desencola por delante y se encola por el final de la lista lo cual tendremos un costo bajo debido
  a la estructura de una lista mientras que la segunda variante es al reves tendra un costo mayor
  al de la primera variante. 
-}
--4|STACK|

--1.
apilar :: [a] -> Stack a
--Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar []     = emptyS
apilar (x:xs) = push x (apilar xs)    

desapilar :: Stack a -> [a]
--Dada una pila devuelve una lista sin alterar el orden de los elementos.
desapilar s = if isEmptyS s
                    then []
                    else top s : desapilar s 

insertarEnPos :: Int -> a -> Stack a -> Stack a
--Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha
--posición (se desapilan elementos hasta dicha posición y se inserta en ese lugar).
insertarEnPos x y s = apilar (agregarEnLaPosicion x y (desapilar s))

agregarEnLaPosicion :: Int -> a -> [a] -> [a]
--La cantidad de elementos de la lista debe ser mayor al numero dado.
agregarEnLaPosicion 0 x xs     = x : xs 
agregarEnLaPosicion y x (x:xs) = agregarEnLaPosicion (y - 1) x xs

--5|QUEUE CON DOS LISTAS|
{-
La ventaja recae en el costo, ya que tendra todas las operaciones constantes pero cada tantas operaciones
se ejecutara una operacion cuadratica, lo cual decimos que tiene todas las operaciones constantes aunque
alguna/s operacion de forma amortizada.
-}