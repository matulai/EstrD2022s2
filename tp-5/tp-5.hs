import SET

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
longitud [] = 0
longitud (x:xs) = 1 + longitud xs
--Costo: Cuadratica O(n^2). Invoca a la funcion "factorial" por cada elemento de la lista dada.
--El peor caso posible es el n mayor de la lista dada.
factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs
--Costo: Lineal O(1). Solo se hacen operaciones de costo constantes por cada elemento.
--Por cada elemento de la lista se hacen dos operaciones (operador de comparacion y operador logico).
pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs
--Costo: Cuadratica O(n^2). Invoca a la funcion "pertenece" por cada elemento de la lista dada.
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) =   if pertenece x xs
                            then sinRepetidos xs
                            else x : sinRepetidos xs
--Costo: Lineal O(n). Depende de la cantidad de elementos de la primera lista.
-- equivalente a (++)
append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x : append xs ys
--Costo: Lineal O(n). Depende de la cantidad de elementos de la lista de string.
concatenar :: [String] -> String
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs
--Costo: Lineal O(n). Peor caso posible que el tamaño de la lista sea mayor o igual a n. 
takeN :: Int -> [a] -> [a]
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs
--Costo: Lineal O(n). Peor caso posible que el tamaño de la lista sea mayor o igual a n. 
dropN :: Int -> [a] -> [a]
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs
--Costo: Lineal O(n)
partir :: Int -> [a] -> ([a], [a])
partir n xs = (takeN n xs, dropN n xs)
--Costo: Cuadratico O(n^2)
minimo :: Ord a => [a] -> a
minimo [x] = x
minimo (x:xs) = min x (minimo xs)
--Costo: Lineal
sacar :: Eq a => a -> [a] -> [a]
sacar n [] = []
sacar n (x:xs) =    if n == x
                        then xs
                        else x : sacar n xs
--Costo: Cuadratico O(n^2)
ordenar :: Ord a => [a] -> [a]
ordenar [] = []
orderar xs =    let m = minimo xs
                    in m : ordenar (sacar m xs)

--2|SET (CONJUNTO)|