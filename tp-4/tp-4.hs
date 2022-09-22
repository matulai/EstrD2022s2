{-	
------------------------------------------------------
||	PRÁCTICA N°4 ||EJERCICIOS INTEGRADORES       	||
||	Alumno: Matias Laime					        ||
||	Fecha De Inicio: 12/09/2022				        ||
------------------------------------------------------
-}
--1.|PIZZA|
data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show
data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int deriving Show
--Para hacer pruebas ---------------------------------
pizza1 = Capa Salsa (Capa (Aceitunas 5) (Capa (Aceitunas 6) (Capa Queso (Capa Salsa Prepizza ))))
--Funciones observadoras------------------------------
pizza :: Pizza -> Pizza
pizza (Capa _ p) = p
pizza _          = Prepizza

ingrediente :: Pizza -> Ingrediente
ingrediente (Capa i _) = i
------------------------------------------------------
cantidadDeCapas :: Pizza -> Int 
cantidadDeCapas Prepizza   = 0
cantidadDeCapas p = 1 + cantidadDeCapas (pizza p)
------------------------------------------------------
armarPizza :: [Ingrediente] -> Pizza
armarPizza []     = Prepizza
armarPizza (i:is) = Capa i (armarPizza is) 
------------------------------------------------------
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon p = if (sonIngredientesIguales (ingrediente p) Jamon) 
                            then sacarJamon (pizza p) 
                            else Capa (ingrediente p) (sacarJamon (pizza p))

sonIngredientesIguales :: Ingrediente -> Ingrediente -> Bool
sonIngredientesIguales Salsa         Salsa         = True
sonIngredientesIguales Queso         Queso         = True
sonIngredientesIguales Jamon         Jamon         = True
sonIngredientesIguales (Aceitunas _) (Aceitunas _) = True
sonIngredientesIguales _             _             = False 
------------------------------------------------------
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza   = True 
tieneSoloSalsaYQueso p = (sonIngredientesIguales (ingrediente p) Salsa || sonIngredientesIguales (ingrediente p) Queso) 
                                && tieneSoloSalsaYQueso (pizza p)

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza   = Prepizza
duplicarAceitunas p          = Capa (duplicarSiSonAceitunas (ingrediente p)) (duplicarAceitunas (pizza p))

duplicarSiSonAceitunas :: Ingrediente -> Ingrediente
duplicarSiSonAceitunas (Aceitunas x) = Aceitunas (x * 2)
duplicarSiSonAceitunas i             = i
------------------------------------------------------
cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
cantCapasPorPizza []     = []
cantCapasPorPizza (p:ps) = (cantidadDeCapas p, p) : cantCapasPorPizza ps 
--2.|MAPA DE TESOROS (con bifurcación)|
data Dir = Izq | Der deriving Show
data Objeto = Tesoro | Chatarra deriving Show
data Cofre = Cofre [Objeto] deriving Show
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa deriving Show
--Funciones observadoras------------------------------
objetosEn :: Cofre -> [Objeto]
objetosEn (Cofre x) = x
------------------------------------------------------
hayTesoro :: Mapa -> Bool
hayTesoro (Fin c)               = contieneTesoro (objetosEn c)
hayTesoro (Bifurcacion c m1 m2) = contieneTesoro (objetosEn c) || hayTesoro m1 || hayTesoro m2 

contieneTesoro :: [Objeto] -> Bool
contieneTesoro []     = False
contieneTesoro (x:xs) = esTesoro x || contieneTesoro xs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False
------------------------------------------------------
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn []     m       = contieneTesoro (objetosDelCofreEn m)
hayTesoroEn _      (Fin _) = False
hayTesoroEn (d:ds) m       = hayTesoroEn ds (irPorElCamino d m)

irPorElCamino :: Dir -> Mapa -> Mapa
irPorElCamino _   (Fin m)              = Fin m
irPorElCamino Izq (Bifurcacion c m1 _) = m1
irPorElCamino Der (Bifurcacion c _ m2) = m2

objetosDelCofreEn :: Mapa -> [Objeto]
objetosDelCofreEn (Fin c)             = objetosEn c
objetosDelCofreEn (Bifurcacion c _ _) = objetosEn c
------------------------------------------------------
-- caminoAlTesoro :: Mapa -> [Dir]
-- --PRECONDICION: Existe un tesoro y es unico.
-- caminoAlTesoro (Fin c)               =
-- caminoAlTesoro (Bifurcacion c m1 m2) =  

-- irHaciaLaDerecha

{-
caminoAlTesoro :: Mapa -> [Dir]
caminoAlTesoro m =
            case (caminoAlTesoro'' m) of
                (Just indicacion) -> indicacion 
                Nothing -> error "no hay tesoro"

caminoAlTesoro'' :: Mapa -> Maybe [Dir]
caminoAlTesoro'' (Fin c) = 
            if hayTesoroEnCofre c
                then Just []
                else Nothing
caminoAlTesoro'' (Bifurcacion c m1 m2) = 
            if hayTesoroEnCofre c 
                then  
                else
-}
------------------------------------------------------
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin _)               = []
caminoDeLaRamaMasLarga (Bifurcacion _ m1 m2) = caminoMasLargo (Izq : caminoDeLaRamaMasLarga m1) (Der : caminoDeLaRamaMasLarga m2)

caminoMasLargo :: [a] -> [a] -> [a]
caminoMasLargo l1 l2 = if length l1 >= length l2 then l1 else l2
------------------------------------------------------
tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin c)               = [objetosEn c]
tesorosPorNivel (Bifurcacion c m1 m2) = objetosEn c : juntarNiveles (tesorosPorNivel m1) (tesorosPorNivel m2)

juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles xss      _        = xss
juntarNiveles _        yss      = yss
juntarNiveles (xs:xss) (ys:yss) = (xs ++ ys) : juntarNiveles xss yss
------------------------------------------------------
todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin _)               = []
todosLosCaminos (Bifurcacion _ m1 m2) = agregarATodos Izq (todosLosCaminos m1) ++ agregarATodos Der (todosLosCaminos m2)

agregarATodos :: a -> [[a]] -> [[a]]
agregarATodos x _      = []
agregarATodos x (ys:yss) = (x : ys) : agregarATodos x yss 
------------------------------------------------------
--3.|NAVE ESPACIAL|
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril] deriving Show
data Barril = Comida | Oxigeno | Torpedo | Combustible deriving Show
data Sector = S SectorId [Componente] [Tripulante] deriving Show
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show
data Nave = N (Tree Sector) deriving Show
--Para hacer pruebas----------------------------------
nave = N (NodeT sector11 
            (NodeT sector22A 
                (NodeT sector43A 
                    EmptyT 
                    (NodeT sector84A EmptyT EmptyT)) 
                (NodeT sector53A 
                    (NodeT sector94A 
                        EmptyT
                        (NodeT sector155A EmptyT EmptyT)) 
                    (NodeT sector104A 
                        (EmptyT) 
                        (NodeT sector165A EmptyT EmptyT)))) 

            (NodeT sector32B
                (NodeT sector63B 
                    (NodeT sector114B 
                        (NodeT sector175B EmptyT EmptyT) 
                        EmptyT) 
                    (NodeT sector124B EmptyT EmptyT)) 
                (NodeT sector73B 
                    (NodeT sector134B EmptyT EmptyT) 
                    (NodeT sector144B EmptyT EmptyT))))
{-
El izquierdo de un nodo Par sera un nodo Impar y el derecho uno Par.
El izquierdo de un nodo Impar sera un nodo Par y el derecho uno Impar.
                     /------------------11A------------------\
             /------22A------\                       /------32B------\
         /--43A          /--53A--\               /--63B--\       /--73B--\
        84A          /--94A  /--104A         /--114B    124B    134B    144B
                    155A    165A            175B  
-} 

sector11 = S id1 [(Almacen barriles1)] [tripulante1]

sector22A = S id2 [Almacen barriles2] [tripulante2]
sector43A = S id4 [Almacen barriles3] [tripulante3]
sector53A = S id5 [Almacen barriles4] [tripulante3]
sector84A = S id8 [Motor 15] [tripulante4]
sector94A = S id9 [Motor 65] [tripulante4]
sector104A = S id10 [Motor 20] [tripulante4]
sector155A = S id15 [LanzaTorpedos] [tripulante4, tripulante1]
sector165A = S id16 [LanzaTorpedos] [tripulante4, tripulante1]

sector32B = S id3 [Almacen barriles2] [tripulante2]
sector63B = S id6 [Almacen barriles3] [tripulante3]
sector73B = S id7 [Almacen barriles4] [tripulante3]
sector114B = S id11 [Motor 15] [tripulante4]
sector124B = S id12 [Motor 65] [tripulante4]
sector134B = S id13 [Motor 20] [tripulante4]
sector144B = S id14 [LanzaTorpedos] [tripulante4, tripulante1]
sector175B = S id17 [LanzaTorpedos] [tripulante4]

tripulante1 = "yo"
tripulante2 = "tu"
tripulante3 = "el"
tripulante4 = "ellos"

id1 = "id1"

id2 = "id22A"
id4 = "id43A"
id5 = "id53A"
id8 = "id84A"
id9 = "id94A"
id10 = "id104A"
id15 = "id155A"
id16 = "id165A"

id3 = "id32B"
id6 = "id63B"
id7 = "id73B"
id11 = "id114B"
id12 = "id124B"
id13 = "id134B"
id14 = "id145B"
id17 = "id175B"

barriles1 = [Comida, Oxigeno, Torpedo, Combustible]
barriles2 = [Comida, Comida, Comida, Comida]
barriles3 = [Oxigeno, Oxigeno, Oxigeno, Oxigeno]
barriles4 = [Torpedo, Torpedo, Combustible, Combustible]
--Funciones Observadoras------------------------------
idDelSector :: Sector -> SectorId
idDelSector (S id _ _) = id  

treeDe :: Nave -> Tree Sector
treeDe (N t) = t 

componentesEn :: Sector -> [Componente] 
componentesEn (S _ c _) = c

poderDePropulsionDe :: Componente -> Int
poderDePropulsionDe (Motor p)  = p
poderDePropulsionDe _          = 0

barrilesEnAlmacen :: Componente -> [Barril]
barrilesEnAlmacen (Almacen bs) = bs

tripulantesEn :: Sector -> [Tripulante]
tripulantesEn (S _ _ ts) = ts
------------------------------------------------------
sectores :: Nave -> [SectorId]
sectores n = idDeSectoresEn (treeDe n)

idDeSectoresEn :: Tree Sector -> [SectorId]
idDeSectoresEn EmptyT        = []
idDeSectoresEn (NodeT s t1 t2) = idDelSector s : idDeSectoresEn t1 ++ idDeSectoresEn t2
-- idDeSectoresEn (NodeT x y z) = idDelSector x : ordenarPrimeroL1LuegoPrimeroL2 (idDeSectoresEn y) (idDeSectoresEn z)

ordenarPrimeroL1LuegoPrimeroL2 :: [a] -> [a] -> [a]
ordenarPrimeroL1LuegoPrimeroL2 xs     []     = xs
ordenarPrimeroL1LuegoPrimeroL2 []     ys     = ys
ordenarPrimeroL1LuegoPrimeroL2 (x:xs) (y:ys) = (x : y : []) ++ ordenarPrimeroL1LuegoPrimeroL2 xs ys
------------------------------------------------------
poderDePropulsion :: Nave -> Int
poderDePropulsion n = propulsionDeLaNave (treeDe n)

propulsionDeLaNave :: Tree Sector -> Int
propulsionDeLaNave EmptyT           = 0
propulsionDeLaNave (NodeT s t1 t2)  = poderDePropulsionEnElSector (componentesEn s) + propulsionDeLaNave t1 + propulsionDeLaNave t2

poderDePropulsionEnElSector :: [Componente] -> Int
poderDePropulsionEnElSector []     = 0
poderDePropulsionEnElSector (c:cs) = poderDePropulsionDe c + poderDePropulsionEnElSector cs
------------------------------------------------------
barriles :: Nave -> [Barril]
barriles n = barrilesEnLaNave (treeDe n)

barrilesEnLaNave :: Tree Sector -> [Barril]
barrilesEnLaNave EmptyT          = []
barrilesEnLaNave (NodeT s t1 t2) = barrilesEnElSector (componentesEn s) ++ barrilesEnLaNave t1 ++ barrilesEnLaNave t2

barrilesEnElSector :: [Componente] -> [Barril]
barrilesEnElSector []     = []
barrilesEnElSector (c:cs) = if (esAlmacen c)
                                then barrilesEnAlmacen c ++ barrilesEnElSector cs
                                else barrilesEnElSector cs

esAlmacen :: Componente -> Bool
esAlmacen (Almacen _) = True
esAlmacen _           = False
------------------------------------------------------
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs sid n = N (agregar_AlSector_DeLaNave cs sid (treeDe n))

agregar_AlSector_DeLaNave :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregar_AlSector_DeLaNave _  _   EmptyT          = EmptyT
agregar_AlSector_DeLaNave cs sid (NodeT s t1 t2) = if (sid == idDelSector s)
                                                        then NodeT (S sid (cs ++ (componentesEn s)) (tripulantesEn s)) t1 t2
                                                        else NodeT s (agregar_AlSector_DeLaNave cs sid t1) (agregar_AlSector_DeLaNave cs sid t2)
------------------------------------------------------
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave

------------------------------------------------------
sectoresAsignados :: Tripulante -> Nave -> [SectorId]

------------------------------------------------------
tripulantes :: Nave -> [Tripulante]

--4.|MANADA DE LOBOS|
type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre
data Manada = M Lobo
--Funciones Observadoras------------------------------
nombreDe :: Lobo -> Nombre
nombreDe (Cria n)             = n
nombreDe (Cazador n _ _ _ _)  = n
nombreDe (Explorador n _ _ _) = n
------------------------------------------------------

------------------------------------------------------
