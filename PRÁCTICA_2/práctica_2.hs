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
pertenece x (y : ys) = x == y || pertenece x ys
    --8
apariciones :: Eq a => a -> [a] -> Int
apariciones _ []       = 0
apariciones x (y : ys) = unoSi (x == y) + apariciones x ys
    --9
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _ []       = []
losMenoresA x (y : ys) = elementoListaSi y (x > y) ++ losMenoresA x ys
    --10
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ []       = []
lasDeLongitudMayorA x (y : ys) = elementoListaSi y ((longitud y) > x) ++ lasDeLongitudMayorA x ys
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
-- reversa []       = [] 
-- reversa (x : xs) = reversa xs : x

ultimo :: [a] -> a
--PRECONDICIÓN: La lista no debe ser vacia
ultimo (x : []) = x
ultimo (x : xs) = ultimo xs
    --14
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] []             = []
zipMaximos [] (y : ys)       = y : zipMaximos [] ys
zipMaximos (x : xs) []       = x : zipMaximos [] xs
zipMaximos (x : xs) (y : ys) = primerElementoSi_Sino (x > y) x y : zipMaximos xs ys

primerElementoSi_Sino :: Bool -> a -> a -> a
primerElementoSi_Sino True  a _ = a
primerElementoSi_Sino False _ a = a

    --15
elMinimo :: Ord a => [a] -> a
--PRECONDICIÓN: La lista no debe ser vacia
elMinimo (x : []) = x
elMinimo (x : y : xs) = elMinimo ((elemento_Si_Sino_ x (x < y) y) : xs)

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
mayoresA x (y : ys) = elementoListaSi y (x < (edad y)) ++ mayoresA x ys

elementoListaSi :: a -> Bool -> [a]
elementoListaSi a True  = [a]
elementoListaSi _ False = []

promedioEdad :: [Persona] -> Int 
--PRECONDICIÓN : La lista posee al menos una persona
promedioEdad (x : []) = edad x
promedioEdad (x : xs) = div (edad x + promedioEdad xs) 2

elMasViejo :: [Persona] -> Persona
--PRECONDICIÓN : La lista posee al menos una persona
elMasViejo (x : []) = x
elMasViejo (x : y : xs) = elMasViejo ((elemento_Si_Sino_ x (edad x > edad y) y) : xs)
    
elemento_Si_Sino_ :: a -> Bool -> a -> a
elemento_Si_Sino_ a True _  = a
elemento_Si_Sino_ _ False a = a
    --2
data TipoDePokemon = Agua | Fuego | Planta deriving Show
data Pokemon = Pk TipoDePokemon Int
data Entrenador = Ent String [Pokemon]
tipoDePokemonDe :: Pokemon -> TipoDePokemon
tipoDePokemonDe (Pk t e) = t
sonDelMismoTipoDePokemon :: TipoDePokemon -> TipoDePokemon -> Bool
sonDelMismoTipoDePokemon Agua   Agua    = True
sonDelMismoTipoDePokemon Fuego  Fuego   = True
sonDelMismoTipoDePokemon Planta Planta  = True

cantPokemon :: Entrenador -> Int
cantPokemon (Ent _ p) = longitud p

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe tp (Ent _ ps) = cantDePokemonesDe tp ps

cantDePokemonesDe :: TipoDePokemon -> [Pokemon] -> Int
cantDePokemonesDe _ []          = 0
cantDePokemonesDe tp (p : ps)   = unoSi(sonDelMismoTipoDePokemon tp (tipoDePokemonDe p)) + cantDePokemonesDe tp ps

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0

losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
losQueLeGanan tp (Ent _ ps1) (Ent _ ps2) = cantDePokemonesDe tp ps1 - cantDePokemonesDe (esEficazContra tp) ps2 + cantDePokemonesDe tp ps1

esEficazContra :: TipoDePokemon -> TipoDePokemon
esEficazContra  Agua = Fuego
esEficazContra  Fuego = Planta
esEficazContra  Planta = Agua

esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon (Ent _ ps) = hayPokemonDe Agua ps && hayPokemonDe Fuego ps && hayPokemonDe Planta ps

hayPokemonDe :: TipoDePokemon -> [Pokemon] -> Bool
hayPokemonDe _ [] = False
hayPokemonDe tp (p : ps) = sonDelMismoTipoDePokemon tp (tipoDePokemonDe p) || hayPokemonDe tp ps

    --3
data Seniority = Junior | SemiSenior | Senior
data Proyecto = Pry String
data Rol = Dev Seniority Proyecto | Mng Seniority Proyecto
data Empresa = Emp [Rol]

-- proyectos :: Empresa -> [Proyecto]

-- losDevSenior :: Empresa -> [Proyecto] -> Int

-- cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int

-- asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
