{-	
------------------------------------------------------
||	PRÁCTICA N°2 |LISTAS Y RECURSIÓN ESTRUCTURAL|	||
||	Alumno: Matias Laime					        ||
||	Fecha De Inicio: 19/08/2022				        ||
------------------------------------------------------
-}
--1.|RECURSIÓN SOBRE LISTAS|
    --1
sumatoria :: [Int] -> Int 
sumatoria []       = 0
sumatoria (x : xs) = x + sumatoria xs
    --2
longitud :: [a] -> Int
longitud []       = 0
longitud (_ : xs) = 1 + longitud xs
    --3
sucesores ::  [Int] -> [Int]
sucesores []       = []
sucesores (x : xs) = x - 1 : sucesores xs
    --4
conjuncion :: [Bool] -> Bool
conjuncion (x : []) = x
conjuncion (x : xs) = x && conjuncion xs
    --5
disyuncion :: [Bool] -> Bool
disyuncion (x : []) = x
disyuncion (x : xs) = x || disyuncion xs 
    --6
aplanar :: [[a]] -> [a]
aplanar []       = []
aplanar (x : xs) = x ++ aplanar xs
    --7
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ []       = False
pertenece x (y : ys) = if x == y 
                            then True
                            else pertenece x ys 
    --8
apariciones :: Eq a => a -> [a] -> Int
apariciones _ []       = 0
apariciones x (y : ys) = if x == y
                            then 1 + apariciones x ys
                            else apariciones x ys
    --9
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _ []       = []
losMenoresA x (y : ys) = if x > y
                            then y : losMenoresA x ys
                            else losMenoresA x ys
    --10
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ []       = []
lasDeLongitudMayorA x (y : ys) = if (longitud y) > x
                                    then y : lasDeLongitudMayorA x ys 
                                    else lasDeLongitudMayorA x ys 
    --11
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] x       = x : []
agregarAlFinal (y : ys) x =  y : agregarAlFinal ys x
    --12
agregar :: [a] -> [a] -> [a]
agregar []       ys = ys
agregar (x : xs) ys = x : agregar xs ys
    --13
-- reversa :: [a] -> [a]
-- reversa (x:[])   = [] : x
-- reversa (x : xs) = reversa xs : x
    --14
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] []             = []
zipMaximos [] (y : ys)       = y : zipMaximos [] ys
zipMaximos (x : xs) []       = x : zipMaximos [] xs
zipMaximos (x : xs) (y : ys) = if x > y
                               then x : zipMaximos xs ys
                               else y : zipMaximos xs ys
    --15
-- elMinimo :: Ord a => [a] -> a
-- elMinimo     =

--2.|RECURSIÓN SOBRE NÚMEROS|
    --1
factorial :: Int -> Int
factorial 0 = 1
factorial x = x * factorial (x - 1)
    --2
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = []
cuentaRegresiva x = x : cuentaRegresiva (x - 1)
    --3
repetir :: Int -> a -> [a]
repetir 0 _ = []
repetir x y = y : repetir (x - 1) y
    --4
losPrimeros :: Int -> [a] -> [a]
losPrimeros _ []       = []
losPrimeros 0 _        = []
losPrimeros x (y : ys) = y : losPrimeros (x - 1) ys 

sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 y        = y
sinLosPrimeros _ []       = []
sinLosPrimeros x (y : ys) = sinLosPrimeros (x - 1) ys 

--3.|REGISTROS|
    --1
data Persona = P String Int 
edad :: Persona -> Int
edad (P s i) = i

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ []       = []
mayoresA x (y : ys) = if x > (edad y)
                      then mayoresA x ys 
                      else y : mayoresA x ys

promedioEdad :: [Persona] -> Int 
--PRECONDICIÓN : La lista posee al menos una persona
promedioEdad (x : []) = edad x
promedioEdad (x : xs) = div (edad x + promedioEdad xs) 2

-- elMasViejo :: [Persona] -> Persona
-- --PRECONDICIÓN : La lista posee al menos una persona
-- elMasViejo (x : []) = x
-- elMasViejo (x : y : xs) = if ()
    --2
data TipoDePokemon = Agua | Fuego | Planta deriving Show
data Pokemon = Pk TipoDePokemon Int
data Entrenador = Ent String [Pokemon]
tipoDePokemonDe :: Pokemon -> TipoDePokemon
tipoDePokemonDe (Pk t e) = t
sonDelMismoTipoDePokemon :: TipoDePokemon -> TipoDePokemon -> Bool
sonDelMismoTipoDePokemon Agua Agua = True
sonDelMismoTipoDePokemon Fuego Fuego = True
sonDelMismoTipoDePokemon Planta Planta = True

cantPokemon :: Entrenador -> Int
cantPokemon (Ent s p) = longitud p


-- cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
-- cantPokemonDe x (Ent s (y : ys)) = if (sonDelMismoTipoDePokemon (tipoDePokemonDe y) x)
--                                    then 1 + cantPokemonDe x Ent
--                                    else cantPokemonDe x (Ent s ys )