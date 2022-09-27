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
------------------------------------------------------
cantidadDeCapas :: Pizza -> Int 
cantidadDeCapas Prepizza   = 0
cantidadDeCapas (Capa _ p) = 1 + cantidadDeCapas p
------------------------------------------------------
armarPizza :: [Ingrediente] -> Pizza
armarPizza []     = Prepizza
armarPizza (i:is) = Capa i (armarPizza is) 
------------------------------------------------------
sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza   = Prepizza
sacarJamon (Capa i p) = if (esIngrediente Jamon i) 
                            then sacarJamon p
                            else Capa i (sacarJamon p)

esIngrediente :: Ingrediente -> Ingrediente -> Bool
esIngrediente Salsa         Salsa         = True
esIngrediente Queso         Queso         = True
esIngrediente Jamon         Jamon         = True
esIngrediente (Aceitunas _) (Aceitunas _) = True
esIngrediente _             _             = False 
------------------------------------------------------
tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza   = True 
tieneSoloSalsaYQueso (Capa i p) = (esIngrediente Salsa i || esIngrediente Queso i) 
                                && tieneSoloSalsaYQueso p
------------------------------------------------------
duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza   = Prepizza
duplicarAceitunas (Capa i p) = if esIngrediente (Aceitunas 0) i 
                                    then Capa (duplicarCapaDeAceitunas i) (duplicarAceitunas p)
                                    else Capa i (duplicarAceitunas p)

duplicarCapaDeAceitunas :: Ingrediente -> Ingrediente
duplicarCapaDeAceitunas (Aceitunas x) = Aceitunas (x * 2)
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
------------------------------------------------------
mapa = Bifurcacion cofre2 
            (Bifurcacion cofre2 
                (Bifurcacion cofre2 (Fin cofre2) (Fin cofre2)) 
                (Bifurcacion cofre2 (Fin cofre2) (Fin cofre2))) 
            (Bifurcacion cofre2
                (Bifurcacion cofre2 (Fin cofre2) (Fin cofre1)) 
                (Bifurcacion cofre2 (Fin cofre2) (Fin cofre2)))  

cofre1 = Cofre [Tesoro]
cofre2 = Cofre [Chatarra]


--Funciones observadoras------------------------------
objetosEn :: Cofre -> [Objeto]
objetosEn (Cofre x) = x

cofreDe :: Mapa -> Cofre
cofreDe (Fin c)             = c
cofreDe (Bifurcacion c _ _) = c
------------------------------------------------------
hayTesoro :: Mapa -> Bool
hayTesoro (Fin c)               = contieneTesoro c
hayTesoro (Bifurcacion c m1 m2) = contieneTesoro c || hayTesoro m1 || hayTesoro m2 

contieneTesoro :: Cofre -> Bool
contieneTesoro (Cofre obj) = hayTesoroEnObjetos obj 

hayTesoroEnObjetos :: [Objeto] -> Bool
hayTesoroEnObjetos []     = False
hayTesoroEnObjetos (x:xs) = esTesoro x || hayTesoroEnObjetos xs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False
------------------------------------------------------
hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn []     m       = contieneTesoro (cofreDe m)
hayTesoroEn _      (Fin _) = False
hayTesoroEn (d:ds) m       = hayTesoroEn ds (irPorElCamino d m)

irPorElCamino :: Dir -> Mapa -> Mapa
irPorElCamino _   (Fin m)              = Fin m
irPorElCamino Izq (Bifurcacion c m1 _) = m1
irPorElCamino Der (Bifurcacion c _ m2) = m2
------------------------------------------------------
caminoAlTesoro :: Mapa -> [Dir]
--PRECONDICION: Existe un tesoro y es unico.
caminoAlTesoro (Fin c)               = []
caminoAlTesoro (Bifurcacion c m1 m2) = if (contieneTesoro c)
                                        then []
                                        else if (hayTesoro m1)
                                            then Izq : caminoAlTesoro m1
                                            else Der : caminoAlTesoro m2 
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
id14 = "id144B"
id17 = "id175B"

barriles1 = [Comida, Oxigeno, Torpedo, Combustible]
barriles2 = [Comida, Comida, Comida, Comida]
barriles3 = [Oxigeno, Oxigeno, Oxigeno, Oxigeno]
barriles4 = [Torpedo, Torpedo, Combustible, Combustible]
--Funciones Observadoras------------------------------
idDelSector :: Sector -> SectorId
idDelSector (S id _ _) = id  

componentesEn :: Sector -> [Componente] 
componentesEn (S _ c _) = c

poderDePropulsionDeComponentes :: Componente -> Int
poderDePropulsionDeComponentes (Motor p)  = p
poderDePropulsionDeComponentes _          = 0

barrilesEnAlmacen :: Componente -> [Barril]
barrilesEnAlmacen (Almacen bs) = bs

tripulantesEn :: Sector -> [Tripulante]
tripulantesEn (S _ _ ts) = ts
------------------------------------------------------
sectores :: Nave -> [SectorId]
sectores (N n) = idDeSectoresEn n

idDeSectoresEn :: Tree Sector -> [SectorId]
idDeSectoresEn EmptyT        = []
idDeSectoresEn (NodeT s t1 t2) = idDelSector s : idDeSectoresEn t1 ++ idDeSectoresEn t2
------------------------------------------------------
poderDePropulsion :: Nave -> Int
poderDePropulsion (N n) = propulsionDeLaNave n

propulsionDeLaNave :: Tree Sector -> Int
propulsionDeLaNave EmptyT           = 0
propulsionDeLaNave (NodeT s t1 t2)  = poderDePropulsionDe (componentesEn s) + propulsionDeLaNave t1 + propulsionDeLaNave t2

poderDePropulsionDe :: [Componente] -> Int
poderDePropulsionDe []     = 0
poderDePropulsionDe (c:cs) = poderDePropulsionDeComponentes c + poderDePropulsionDe cs
------------------------------------------------------
barriles :: Nave -> [Barril]
barriles (N n) = barrilesEnLaNave n

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
agregarASector cs id (N n) = N (agregar_AlSector_DeLaNave cs id n)

agregar_AlSector_DeLaNave :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregar_AlSector_DeLaNave _  _   EmptyT         = EmptyT
agregar_AlSector_DeLaNave cs id (NodeT s t1 t2) = if (id == idDelSector s)
                                                        then NodeT (S id (cs ++ (componentesEn s)) (tripulantesEn s)) t1 t2
                                                        else NodeT s (agregar_AlSector_DeLaNave cs id t1) (agregar_AlSector_DeLaNave cs id t2)
------------------------------------------------------
asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
--PRECONDICIÓN: Todos los id de la lista existen en la nave.
asignarTripulanteA t ids (N n) = N (asignarTripulanteALosSectores t ids n)

asignarTripulanteALosSectores :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarTripulanteALosSectores _ _   EmptyT         = EmptyT
asignarTripulanteALosSectores t id (NodeT s t1 t2) = NodeT (asignarTripulanteS t id s) 
                                                           (asignarTripulanteALosSectores t id t1)
                                                           (asignarTripulanteALosSectores t id t2)

asignarTripulanteS :: Tripulante -> [SectorId] -> Sector -> Sector
asignarTripulanteS t ids (S id cs ts) = 
            if pertenece id ids
                then S id cs (t : ts) 
                else S id cs ts 
------------------------------------------------------
sectoresAsignados :: Tripulante -> Nave -> [SectorId]
sectoresAsignados t (N n) = sectoresAsignadosN t n 

sectoresAsignadosN :: Tripulante -> Tree Sector -> [SectorId]
sectoresAsignadosN _ EmptyT          = []
sectoresAsignadosN t (NodeT s t1 t2) = singularSi (idDelSector s) (estaElTripulanteEn t (tripulantesEn s)) 
                                      ++ sectoresAsignadosN t t1 
                                      ++ sectoresAsignadosN t t2

singularSi :: a -> Bool -> [a]
singularSi a True = [a]
singularSi _ _    = []

estaElTripulanteEn :: Tripulante -> [Tripulante] -> Bool
estaElTripulanteEn _  []       = False 
estaElTripulanteEn t1 (t2:t2s) = t1 == t2 || estaElTripulanteEn t1 t2s
------------------------------------------------------
tripulantes :: Nave -> [Tripulante]
tripulantes (N n) = tripulantesN n 

tripulantesN :: Tree Sector -> [Tripulante]
tripulantesN EmptyT          = []
tripulantesN (NodeT s t1 t2) = agregarTripulanteSinRepetidos (tripulantesEn s) (agregarTripulanteSinRepetidos (tripulantesN t1)  (tripulantesN t2))

agregarTripulanteSinRepetidos :: [Tripulante] -> [Tripulante] -> [Tripulante]
agregarTripulanteSinRepetidos []       ts2 = ts2
agregarTripulanteSinRepetidos (t1:ts1) ts2 = singularSi t1 (estaElTripulanteEn t1 ts2) ++ agregarTripulanteSinRepetidos ts1 ts2
    --                                      if estaElTripulanteEn t1 ts2
    --                                             then agregarTripulanteSinRepetidos ts1 ts2
    --                                             else t1 : agregarTripulanteSinRepetidos ts1 ts2
--4.|MANADA DE LOBOS|
type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre deriving Show
data Manada = M Lobo deriving Show
--Funciones Observadoras------------------------------
nombreDe :: Lobo -> Nombre
nombreDe (Cria n)             = n
nombreDe (Cazador n _ _ _ _)  = n
nombreDe (Explorador n _ _ _) = n
------------------------------------------------------
manada = M lobo

lobo = Cazador "Leonardo" ["conejo", "pato", "pescado", "ave"] 
            (Explorador "David" ["rio", "bosque", "laguna"] 
                (Cria "cria1") 
                (Cria "cria2")) 
            (Explorador "John" ["llanura", "rio", "bosque"] 
                (Cria "cria3") 
                (Cria "cria4")) 
            (Cria "cria5")
------------------------------------------------------
buenaCaza :: Manada -> Bool
buenaCaza (M l) = cantidadDePresas l > cantidadDeCrias l

cantidadDePresas :: Lobo -> Int
cantidadDePresas (Cria _)                = 0
cantidadDePresas (Explorador _ _ l1 l2)  = cantidadDePresas l1 + cantidadDePresas l2 
cantidadDePresas (Cazador _ ps l1 l2 l3) = (length ps) + cantidadDePresas l1 + cantidadDePresas l2 + cantidadDePresas l3

cantidadDeCrias :: Lobo -> Int
cantidadDeCrias (Cria _)               = 1
cantidadDeCrias (Explorador _ _ l1 l2) = cantidadDeCrias l1 + cantidadDeCrias l2
cantidadDeCrias (Cazador _ _ l1 l2 l3) = cantidadDeCrias l1 + cantidadDeCrias l2 + cantidadDeCrias l3
------------------------------------------------------
elAlfa :: Manada -> (Nombre, Int)
elAlfa (M l) = elAlfaM l

elAlfaM :: Lobo -> (Nombre, Int)
elAlfaM (Cria n)                = (n,0)
elAlfaM (Explorador n _ l1 l2)  = elQueTieneMasPresas [(n, 0) ,elAlfaM l1 ,elAlfaM l2]
elAlfaM (Cazador n ps l1 l2 l3) = elQueTieneMasPresas [(n, (length ps)) ,elAlfaM l1 ,elAlfaM l2 ,elAlfaM l3]

elQueTieneMasPresas :: [(Nombre, Int)] -> (Nombre, Int)
elQueTieneMasPresas (x : []) = x
elQueTieneMasPresas (x : y : xs) = if snd x >= snd y
                                        then elQueTieneMasPresas (x : xs)
                                        else elQueTieneMasPresas (y : xs)
------------------------------------------------------                                        
losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron t (M l) = losQueExploraronM t l

losQueExploraronM :: Territorio -> Lobo -> [Nombre]
losQueExploraronM _ (Cria _)                = []
losQueExploraronM t (Cazador _ _ l1 l2 l3)  = losQueExploraronM t l1 
                                            ++ losQueExploraronM t l2 
                                            ++ losQueExploraronM t l3 
losQueExploraronM t (Explorador n ts l1 l2) = singularSi n (pertenece t ts) 
                                            ++ losQueExploraronM t l1 
                                            ++ losQueExploraronM t l2 
------------------------------------------------------
exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
exploradoresPorTerritorio (M n) = exploradoresPorTerritorioM n
 
exploradoresPorTerritorioM :: Lobo -> [(Territorio, [Nombre])]
exploradoresPorTerritorioM (Cria _)                = [] 
exploradoresPorTerritorioM (Cazador _ _ l1 l2 l3)  = unirTerritorios (exploradoresPorTerritorioM l1) 
                                                    (unirTerritorios (exploradoresPorTerritorioM l2) (exploradoresPorTerritorioM l3))
exploradoresPorTerritorioM (Explorador n ts l1 l2) = unirTerritorios (territoriosConExplorador ts [n]) 
                                                    (unirTerritorios (exploradoresPorTerritorioM l1) (exploradoresPorTerritorioM l2))

territoriosConExplorador :: [Territorio] -> [Nombre] -> [(Territorio, [Nombre])] 
territoriosConExplorador []     _ = []
territoriosConExplorador (t:ts) n = (t,n) : territoriosConExplorador ts n

unirTerritorios :: [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
unirTerritorios  []           tns2 = tns2
unirTerritorios ((t,ns):tns1) tns2 = if existeTerritorioEn t tns2
                                    then unirTerritorios tns1 (agregarNombresEn ns t tns2)
                                    else (t,ns) : unirTerritorios tns1 tns2

existeTerritorioEn :: Territorio -> [(Territorio, [Nombre])] -> Bool
existeTerritorioEn _ []       = False
existeTerritorioEn t (t1:t1s) = t == fst t1 || existeTerritorioEn t t1s

agregarNombresEn :: [Nombre] -> Territorio -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
--Sin territorios repetidos.
agregarNombresEn _  _ []       = error "El territorio no existe"
agregarNombresEn ns t ((t1,ns1):tns1) = if t == t1
                                    then (t1,ns1 ++ ns) : tns1
                                    else (t1,ns1) : agregarNombresEn ns t tns1
------------------------------------------------------
superioresDelCazador :: Nombre -> Manada -> [Nombre]
superioresDelCazador n (M l) = hastaElNombre n (superioresDelCazadorL n l)

superioresDelCazadorL :: Nombre -> Lobo -> [Nombre]
--PRECONDICIÓN: hay un cazador con dicho nombre y es único.
superioresDelCazadorL n (Cria n1)               = [n]
superioresDelCazadorL n (Explorador n1 _ l1 l2) = laListaConNombre n [n1 : superioresDelCazadorL n l1,
                                                                      n1 : superioresDelCazadorL n l2]
superioresDelCazadorL n (Cazador n1 _ l1 l2 l3) = laListaConNombre n [n1 : superioresDelCazadorL n l1,
                                                                      n1 : superioresDelCazadorL n l2,
                                                                      n1 : superioresDelCazadorL n l3]

laListaConNombre :: Nombre -> [[Nombre]] -> [Nombre]
laListaConNombre n (ns:nss) = if (pertenece n ns)
                                then ns
                                else laListaConNombre n nss

hastaElNombre :: Nombre -> [Nombre] -> [Nombre]
hastaElNombre n (n1:ns1) = if n == n1
                                then []
                                else n1 : hastaElNombre n  ns1

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ []       = False
pertenece x (y : ys) = x == y || pertenece x ys

-- lineal cuadratica constante| costos invariante de representacion.