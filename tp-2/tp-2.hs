{-	
------------------------------------------------------
||	PRÁCTICA N°2 |LISTAS Y RECURSIÓN ESTRUCTURAL|	||
||	Alumno: Matias Laime					        ||
||	Fecha De Inicio: 28/08/2022				        ||
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
sucesores (x : xs) = x + 1 : sucesores xs
    --4
conjuncion :: [Bool] -> Bool
conjuncion [] = True
conjuncion (x : xs) = x && conjuncion xs
    --5
disyuncion :: [Bool] -> Bool
disyuncion [] = False
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

unoSi :: Bool -> Int
unoSi True  = 1
unoSi False = 0
    --9
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _ []       = []
losMenoresA x (y : ys) = singularSi y (y < x) ++ losMenoresA x ys

singularSi :: a -> Bool -> [a]
singularSi x True = x : []
singularSi _ _    = []
    --10
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ []       = []
lasDeLongitudMayorA x (y : ys) = singularSi y (longitud y > x) ++ lasDeLongitudMayorA x ys
    --11
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal []       y = y : []
agregarAlFinal (x : xs) y = x : agregarAlFinal xs y
    --12
agregar :: [a] -> [a] -> [a]
agregar []       ys = ys
agregar (x : xs) ys = x : agregar xs ys
    --13
reversa :: [a] -> [a]
reversa []       = [] 
reversa (x : xs) = reversa xs ++ [x]
    --14
zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos [] []             = []
zipMaximos (x : xs) []       = x : zipMaximos [] xs
zipMaximos [] (y : ys)       = y : zipMaximos [] ys
zipMaximos (x : xs) (y : ys) = primeroSi_SinoSegundo (x > y) (x,y) : zipMaximos xs ys

primeroSi_SinoSegundo :: Bool -> (a, a) -> a
primeroSi_SinoSegundo True (x,_) = x
primeroSi_SinoSegundo _    (_,y) = y
    --15
elMinimo :: Ord a => [a] -> a
--PRECONDICIÓN: La lista no debe ser vacia
elMinimo (x : []) = x
elMinimo (x : xs) = min x (elMinimo xs)

--2.|RECURSIÓN SOBRE NÚMEROS|
    --1
factorial :: Int -> Int
--PRECONDICIÓN: El entero debe ser mayor o igual a 0.
factorial 0 = 1
factorial x = x * factorial (x - 1)
    --2
cuentaRegresiva :: Int -> [Int]
cuentaRegresiva 0 = 0 : []
cuentaRegresiva x = x : cuentaRegresiva (x - 1)
    --3
repetir :: Int -> a -> [a]
repetir 0 _ = []
repetir x y = y : repetir (x - 1) y
    --4
losPrimeros :: Int -> [a] -> [a]
losPrimeros 0 _        = []
losPrimeros _ []       = []
losPrimeros x (y : ys) = y : losPrimeros (x - 1) ys 

sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros 0 ys       = ys
sinLosPrimeros _ []       = []
sinLosPrimeros x (y : ys) = sinLosPrimeros (x - 1) ys 

--3.|REGISTROS|
    --1
data Persona = P String Int deriving Show
--Para probar--
persona1 = P "Matias" 21
persona2 = P "Leo" 23
persona3 = P "Pablo" 33
persona4 = P "Pepe" 41
persona5 = P "Nahuel" 15
--Funciones Observadoras--
edad :: Persona -> Int
edad (P _ i) = i
-----------------------------------------------------
mayoresA :: Int -> [Persona] -> [Persona]
mayoresA _ []       = []
mayoresA x (p : ps) = singularSi p (edad p > x) ++ mayoresA x ps
-----------------------------------------------------
promedioEdad :: [Persona] -> Int 
--PRECONDICIÓN : La lista no debe ser vacía
promedioEdad ps = div (sumarTodasLasEdades ps) (longitud ps)

sumarTodasLasEdades :: [Persona] -> Int
sumarTodasLasEdades []       = 0
sumarTodasLasEdades (p : ps) = edad p + sumarTodasLasEdades ps 
-----------------------------------------------------
elMasViejo :: [Persona] -> Persona
--PRECONDICIÓN : La lista no debe ser vacía
elMasViejo (x : []) = x
elMasViejo (x : xs) = elMasViejoEntre x (elMasViejo xs)

elMasViejoEntre :: Persona -> Persona -> Persona
elMasViejoEntre p1 p2 = primeroSi_SinoSegundo (edad p1 > edad p2) (p1,p2)
    --2
data TipoDePokemon = Agua | Fuego | Planta deriving Show
data Pokemon = Pk TipoDePokemon Int deriving Show
data Entrenador = Ent String [Pokemon] deriving Show
--Pruebas--
squirtle = Pk Agua 1
charizard = Pk Fuego 2
bulbasour = Pk Planta 3

entrenador1 = Ent "Matias" [squirtle, squirtle, squirtle]
entrenador2 = Ent "Leo" [charizard, charizard, charizard]
entrenador3 = Ent "Pepe" [squirtle, charizard, bulbasour]
--Funciones observadoras--
tipoDe :: Pokemon -> TipoDePokemon
tipoDe (Pk t _) = t

pokemonesDe :: Entrenador -> [Pokemon]
pokemonesDe (Ent _ ps) = ps
-----------------------------------------------------
cantPokemon :: Entrenador -> Int
cantPokemon e = longitud (pokemonesDe e)
-----------------------------------------------------
cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe tp e = longitud (pokemonesDeTipo tp (pokemonesDe e))

pokemonesDeTipo :: TipoDePokemon -> [Pokemon] -> [Pokemon]
pokemonesDeTipo _  []       = []
pokemonesDeTipo tp (p : ps) = singularSi p (sonDelMismoTipoDePokemon tp (tipoDe p)) ++ pokemonesDeTipo tp ps

sonDelMismoTipoDePokemon :: TipoDePokemon -> TipoDePokemon -> Bool
sonDelMismoTipoDePokemon Agua   Agua    = True
sonDelMismoTipoDePokemon Fuego  Fuego   = True
sonDelMismoTipoDePokemon Planta Planta  = True
sonDelMismoTipoDePokemon _      _       = False
-----------------------------------------------------
losQueLeGanan :: TipoDePokemon -> Entrenador -> Entrenador -> Int
losQueLeGanan tp e1 e2 = if sonTodosDelTipo (elTipoConElQueEsEficaz tp) (pokemonesDe e2) then cantPokemonDe tp e1
                                                                                         else 0

elTipoConElQueEsEficaz :: TipoDePokemon -> TipoDePokemon
elTipoConElQueEsEficaz Agua   = Fuego
elTipoConElQueEsEficaz Fuego  = Planta
elTipoConElQueEsEficaz Planta = Agua

sonTodosDelTipo :: TipoDePokemon -> [Pokemon] -> Bool
sonTodosDelTipo _ []       = True
sonTodosDelTipo tp (p : ps) = sonDelMismoTipoDePokemon tp (tipoDe p) && sonTodosDelTipo tp ps

-----------------------------------------------------
esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon e = hayPokemonDe Agua (pokemonesDe e) && 
                     hayPokemonDe Planta (pokemonesDe e) &&
                     hayPokemonDe Fuego (pokemonesDe e)

hayPokemonDe :: TipoDePokemon -> [Pokemon] -> Bool
hayPokemonDe _  []       = False
hayPokemonDe tp (p : ps) = sonDelMismoTipoDePokemon tp (tipoDe p) || hayPokemonDe tp ps
    --3
data Seniority = Junior | SemiSenior | Senior deriving Show
data Proyecto = Pry String deriving Show
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto deriving Show
data Empresa = Emp [Rol] deriving Show
--Para pruebas--
proyecto1 = Pry "Hola"  
proyecto2 = Pry "Chau"

desarrollador1 = Developer Junior proyecto1
desarrollador2 = Developer Senior proyecto2

empresa = Emp [desarrollador1, desarrollador2]
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
-----------------------------------------------------
proyectos :: Empresa -> [Proyecto]
proyectos e = sinProyectosRepetidos (proyectosEn (rol e))

proyectosEn :: [Rol] -> [Proyecto]
proyectosEn []       = []
proyectosEn (r : rs) = proyectoDe r : proyectosEn rs 

sinProyectosRepetidos :: [Proyecto] -> [Proyecto] 
sinProyectosRepetidos []       = []
sinProyectosRepetidos (p : ps) = singularSi p (not (existeProyecto_En p ps)) ++ sinProyectosRepetidos ps

existeProyecto_En :: Proyecto -> [Proyecto] -> Bool
existeProyecto_En _ []         = False
existeProyecto_En p (p1 : p1s) = nombreDeProyecto p == nombreDeProyecto p1 || existeProyecto_En p p1s

-----------------------------------------------------

losDevSenior :: Empresa -> [Proyecto] -> Int
losDevSenior e ps = cantidadTrabajandoEnAlgunoDe (soloDesarrolladoresSenior (rol e)) ps 

cantidadTrabajandoEnAlgunoDe :: [Rol] -> [Proyecto] -> Int
cantidadTrabajandoEnAlgunoDe []       _  = 0
cantidadTrabajandoEnAlgunoDe (r : rs) ps = unoSi (existeProyecto_En (proyectoDe r) ps) + cantidadTrabajandoEnAlgunoDe rs ps

soloDesarrolladoresSenior :: [Rol] -> [Rol]
soloDesarrolladoresSenior []       = []
soloDesarrolladoresSenior (r : rs) = singularSi r (esDesarrollador r && rol_EsSeniority_ r Senior) ++ soloDesarrolladoresSenior rs

rol_EsSeniority_ :: Rol -> Seniority -> Bool
rol_EsSeniority_ r s = igualSeniority (seniorityDe r) s

esDesarrollador :: Rol -> Bool
esDesarrollador (Developer _ _) = True
esDesarrollador _               = False

igualSeniority :: Seniority -> Seniority -> Bool
igualSeniority Senior     Senior     = True
igualSeniority SemiSenior SemiSenior = True
igualSeniority Junior     Junior     = True
igualSeniority _          _          = False

-----------------------------------------------------

-- cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int --
-- Función ya hecha. "cantidadTrabajandoEnAlgunoDe"

-----------------------------------------------------

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto e = proyectoYCantidadTrabajandoEn (proyectos e) (rol e)

proyectoYCantidadTrabajandoEn :: [Proyecto] -> [Rol] -> [(Proyecto, Int)]
proyectoYCantidadTrabajandoEn []       _  = []
proyectoYCantidadTrabajandoEn (p : ps) rs = (p, cantidadTrabajandoEnAlgunoDe rs [p]) : proyectoYCantidadTrabajandoEn ps rs
