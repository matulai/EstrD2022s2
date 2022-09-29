module STACK
    (Stack, emptyS, isEmptyS, push, top, pop, lenS)
where

data Stack a = S [a]
{-
    INVARIANTE DE REPRESENTACIÓN: En S []
        -
        -

    CASOS VALIDOS:
        -
    CASOS INVALIDOS:
        -
-}
emptyS :: Stack a
--Crea una pila vacía.
emptyS = S []

isEmptyS :: Stack a -> Bool
--Dada una pila indica si está vacía.
isEmptyS (S xs) = null xs

push :: a -> Stack a -> Stack a
--Dados un elemento y una pila, agrega el elemento a la pila.
push x (S xs) = S (x:xs)

top :: Stack a -> a
--Dada un pila devuelve el elemento del tope de la pila.
top (S xs) = head x

pop :: Stack a -> Stack a
--Dada una pila devuelve la pila sin el primer elemento.
--PRECONDICION: La pila no debe ser vacia.
pop (S xs) = S (sinElUltimo xs)

sinElUltimo :: [a] -> [a]
--PRECONDICIÓN: La lista no debe ser vacia.
sinElUltimo (_:[]) = []
sinElUltimo (x:xs) = x : sinElUltimo xs

lenS :: Stack a -> Int
--Dada la cantidad de elementos en la pila. Costo: constante.
lenS (S xs) = length xs