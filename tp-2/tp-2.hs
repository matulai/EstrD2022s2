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
--PRECONDICIÓN: La lista no debe tener al menos un elemento.
conjuncion (x : []) = x
conjuncion (x : xs) = x && conjuncion xs
    --5
disyuncion :: [Bool] -> Bool
--PRECONDICIÓN: La lista no debe tener al menos un elemento.
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
losMenoresA x (y : ys) = listaDeElemento_Si_SinoListaVacia y (x > y) ++ losMenoresA x ys
    --10
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ []       = []
lasDeLongitudMayorA x (y : ys) = listaDeElemento_Si_SinoListaVacia y ((longitud y) > x) ++ lasDeLongitudMayorA x ys
    --11
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] x       = x : []
agregarAlFinal (y : ys) x =  y : agregarAlFinal ys x
    --12
agregar :: [a] -> [a] -> [a]
agregar []       ys = ys
agregar (x : xs) ys = x : agregar xs ys
    --13
reversa :: [a] -> [a]
reversa []       = [] 
reversa (x : xs)     = (reversa xs) ++ [x]

ultimo :: [a] -> a
--PRECONDICIÓN: La lista no debe ser vacia
ultimo (x : []) = x
ultimo (x : xs) = ultimo xs
    --14
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] []             = []
zipMaximos [] (y : ys)       = y : zipMaximos [] ys
zipMaximos (x : xs) []       = x : zipMaximos [] xs
zipMaximos (x : xs) (y : ys) = primerElementoSi_Sino x (x < y) y : zipMaximos xs ys

primerElementoSi_Sino :: a -> Bool -> a -> a
primerElementoSi_Sino x True  _ = x
primerElementoSi_Sino _ False x = x

    --15
elMinimo :: Ord a => [a] -> a
--PRECONDICIÓN: La lista no debe ser vacia
elMinimo (x : []) = x
elMinimo (x : y : xs) = elMinimo ((primerElementoSi_Sino x (x < y) y) : xs)

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
edad (P _ i) = i

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ []       = []
mayoresA x (y : ys) = listaDeElemento_Si_SinoListaVacia y (x < (edad y)) ++ mayoresA x ys

promedioEdad :: [Persona] -> Int 
--PRECONDICIÓN : La lista posee al menos una persona
promedioEdad (x : []) = edad x
promedioEdad (x : xs) = div (edad x + promedioEdad xs) 2

elMasViejo :: [Persona] -> Persona
--PRECONDICIÓN : La lista posee al menos una persona
elMasViejo (x : []) = x
elMasViejo (x : y : xs) = elMasViejo ((primerElementoSi_Sino x (edad x > edad y) y) : xs)
    
    --2
data TipoDePokemon = Agua | Fuego | Planta deriving Show
data Pokemon = Pk TipoDePokemon Int
data Entrenador = Ent String [Pokemon]
--Funciones observadoras--
tipoDePokemonDe :: Pokemon -> TipoDePokemon
tipoDePokemonDe (Pk t _) = t

pokemonesDe :: Entrenador -> [Pokemon]
pokemonesDe (Ent _ ps) = ps
--------------------------

cantPokemon :: Entrenador -> Int
cantPokemon e = longitud (pokemonesDe e)
--------------------------
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe tp e = cantDePokemonesDeTipo_En_ tp (pokemonesDe e)

cantDePokemonesDeTipo_En_ :: TipoDePokemon -> [Pokemon] -> Int
cantDePokemonesDeTipo_En_ _  []         = 0
cantDePokemonesDeTipo_En_ tp (p : ps)   = unoSi(sonDelMismoTipoDePokemon tp (tipoDePokemonDe p)) + cantDePokemonesDeTipo_En_ tp ps

sonDelMismoTipoDePokemon :: TipoDePokemon -> TipoDePokemon -> Bool
sonDelMismoTipoDePokemon Agua   Agua    = True
sonDelMismoTipoDePokemon Fuego  Fuego   = True
sonDelMismoTipoDePokemon Planta Planta  = True

unoSi :: Bool -> Int
unoSi True = 1
unoSi False = 0
--------------------------
-- losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
-- losQueLeGanan tp (Ent _ ps1) (Ent _ ps2) = cantDePokemonesDeTipo_En_ tp ps1 - cantDePokemonesDeTipo_En_ (esEficazContra tp) ps2 + cantDePokemonesDeTipo_En_ tp ps1

-- esEficazContra :: TipoDePokemon -> TipoDePokemon
-- esEficazContra  Agua    = Fuego
-- esEficazContra  Fuego   = Planta
-- esEficazContra  Planta  = Agua
-- --------------------------
-- esMaestroPokemon :: Entrenador -> Bool
-- esMaestroPokemon e = hayDeTodosLosTiposDePokemon Agua (pokemonesDe e)

-- hayDeTodosLosTiposDePokemon :: Pokemon -> [Pokemon] -> Bool
-- hayDeTodosLosTiposDePokemon 
-- hayDeTodosLosTiposDePokemon p ps = hayPokemonDe p ps && hayPokemonDe siguienteTipoDePokemon 

-- siguienteTipoDePokemon :: TipoDePokemon -> TipoDePokemon
-- siguienteTipoDePokemon Planta = Agua
-- siguienteTipoDePokemon Agua   = Fuego 
-- siguienteTipoDePokemon Fuego  = Planta

-- hayPokemonDe :: TipoDePokemon -> [Pokemon] -> Bool
-- hayPokemonDe _      []        = False
-- hayPokemonDe Planta _         = False
-- hayPokemonDe tp     (p : ps)  = sonDelMismoTipoDePokemon tp (tipoDePokemonDe p) || hayPokemonDe tp ps

    --3
data Seniority = Junior | SemiSenior | Senior deriving Show
data Proyecto = Pry String
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
data Empresa = Emp [Rol]

--------------------------

proyectos :: Empresa -> [Proyecto]
proyectos e = sinProyectosRepetidos (proyectosEn (rol e))

proyectosEn :: [Rol] -> [Proyecto]
proyectosEn []       = []
proyectosEn (r : rs) = (proyectoDe r) : proyectosEn rs 

sinProyectosRepetidos :: [Proyecto] -> [Proyecto] 
sinProyectosRepetidos (p : ps) = listaDeElemento_Si_SinoListaVacia p (existeProyectoEn p ps) ++ sinProyectosRepetidos ps      

listaDeElemento_Si_SinoListaVacia :: a -> Bool -> [a]
listaDeElemento_Si_SinoListaVacia a True  = [a]
listaDeElemento_Si_SinoListaVacia _ _     = []

existeProyectoEn :: Proyecto -> [Proyecto] -> Bool
--PRECONDICIÓN: La Lista no debe tener al menos un elemento.
existeProyectoEn _ []         = True
existeProyectoEn p (p1 : p1s) = (nombreDeProyecto p == (nombreDeProyecto p1)) && existeProyectoEn p p1s

--------------------------

losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior e ps = cantidadTrabajandoEnAlgunoDe (soloDesarrolladoresSenior (rol e)) ps 

cantidadTrabajandoEnAlgunoDe :: [Rol] -> [Proyecto] -> Int
cantidadTrabajandoEnAlgunoDe []       _  = 0
cantidadTrabajandoEnAlgunoDe (r : rs) ps = unoSi (existeProyectoEn (proyectoDe r) ps) + cantidadTrabajandoEnAlgunoDe rs ps

soloDesarrolladoresSenior :: [Rol] -> [Rol]
soloDesarrolladoresSenior []       = []
soloDesarrolladoresSenior (r : rs) = listaDeElemento_Si_SinoListaVacia r (esDesarrollador r && esSeniority_ r Senior ) ++ soloDesarrolladoresSenior rs 

esSeniority_ :: Rol -> Seniority -> Bool
esSeniority_ r s = igualSeniority (seniorityDe r) s

esDesarrollador :: Rol -> Bool
esDesarrollador (Developer _ _) = True
esDesarrollador _               = False

igualSeniority :: Seniority -> Seniority -> Bool
igualSeniority Senior     Senior     = True
igualSeniority SemiSenior SemiSenior = True
igualSeniority Junior     Junior     = True
igualSeniority _          _          = False

--------------------------

-- cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int --
-- Función ya hecha. "cantidadTrabajandoEnAlgunoDe"

--------------------------

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto e = proyectoYCantidadTrabajandoEn (proyectos e) (rol e)

proyectoYCantidadTrabajandoEn :: [Proyecto] -> [Rol] -> [(Proyecto, Int)]
proyectoYCantidadTrabajandoEn []       _  = []
proyectoYCantidadTrabajandoEn (p : ps) rs = (p, cantidadTrabajandoEnAlgunoDe rs [p]) : proyectoYCantidadTrabajandoEn ps rs

--Funciones Observadoras--
nombreDeProyecto :: Proyecto -> String
nombreDeProyecto (Pry n) = n 

proyectoDe :: Rol -> Proyecto
proyectoDe (Developer _ p) = p 
proyectoDe (Management _ p) = p

rol :: Empresa -> [Rol] 
rol (Emp r) = r 

seniorityDe :: Rol -> Seniority
seniorityDe (Developer s _) = s