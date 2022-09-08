{-	
------------------------------------------------------
||	PRÁCTICA N°3 ||TIPOS ALGEBRAICOS RECURSIVOS 	||
||	Alumno: Matias Laime					        ||
||	Fecha De Inicio: 05/09/2022				        ||
------------------------------------------------------
-}
unoSi :: Bool -> Int
unoSi False = 0
unoSi True  = 1
--1.|TIPOS RECURSIVOS SIMPLES|
    --1.1
data Color = Azul | Rojo deriving Show
data Celda = Bolita Color Celda | CeldaVacia deriving Show
--Para hacer pruebas--
celda1 = Bolita Rojo (Bolita Rojo (Bolita Azul (Bolita Azul CeldaVacia)))
celda2 = Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
------------------------------------------------------
nroBolitas :: Color -> Celda -> Int
nroBolitas _    CeldaVacia   = 0
nroBolitas x    (Bolita y z) = unoSi(sonColoresIguales x y) + nroBolitas x z

sonColoresIguales :: Color -> Color -> Bool
sonColoresIguales Azul Azul = True
sonColoresIguales Rojo Rojo = True
sonColoresIguales _    _    = False
------------------------------------------------------
poner :: Color -> Celda -> Celda
poner x CeldaVacia   = Bolita x CeldaVacia
poner x y            = Bolita x y
------------------------------------------------------
sacar :: Color -> Celda -> Celda
sacar _   CeldaVacia = CeldaVacia
sacar x (Bolita y z) = if (sonColoresIguales x y) then z  
                                                  else Bolita y (sacar x z)
------------------------------------------------------
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _  c          = c
ponerN _ _  CeldaVacia = CeldaVacia
ponerN x cl c          = Bolita cl (ponerN (x - 1) cl c)
-- poner cl (ponerN (x - 1) cl c)
-- ponerN x cl c = ponerN (x - 1) cl (poner cl c) 
------------------------------------------------------
    --1.2
data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
--Para hacer pruebas--
camino = Nada (Cofre [Cacharro, Cacharro] (Nada (Cofre [Cacharro, Tesoro] Fin))) 
------------------------------------------------------
hayTesoro :: Camino -> Bool
hayTesoro Fin           = False
hayTesoro (Nada x)      = False || hayTesoro x
hayTesoro (Cofre y x)   = hayTesoroEnObjetos y || hayTesoro x

hayTesoroEnObjetos :: [Objeto] -> Bool
hayTesoroEnObjetos []       = False
hayTesoroEnObjetos (x : xs) = sonObjetosIguales x Tesoro || hayTesoroEnObjetos xs

sonObjetosIguales :: Objeto -> Objeto -> Bool
sonObjetosIguales Cacharro Cacharro = True
sonObjetosIguales Tesoro   Tesoro   = True
sonObjetosIguales _        _        = False
------------------------------------------------------
pasosHastaTesoro :: Camino -> Int
--PRECONDICION: Debe haber al menos un tesoro. 
pasosHastaTesoro (Nada x)    = 1 + pasosHastaTesoro x
pasosHastaTesoro (Cofre x y) = unoSi (not (hayTesoroEnObjetos x)) + pasosHastaTesoro y
------------------------------------------------------
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn x y = if (hayTesoro y) then (pasosHastaTesoro y <= x) 
                                   else False
------------------------------------------------------
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros 0 _           = True
alMenosNTesoros x Fin         = False
alMenosNTesoros x (Nada y)    = True && alMenosNTesoros x y
alMenosNTesoros x (Cofre y z) = True && alMenosNTesoros (x - unoSi(hayTesoroEnObjetos y)) z
------------------------------------------------------
-- cantTesorosEntre :: Int -> Int -> Camino -> Int
-- cantTesorosEntre _ 0 _           = 0
-- cantTesorosEntre _ _ Fin         = 0
-- cantTesorosEntre x y (Nada z)    = 0 + cantTesorosEntre (x - 1) y z
-- cantTesorosEntre x y (Cofre z c) = verSiHayTesoroSi c (x <= 0) : cantTesorosEntre 

-- cantTesorosEntre x y (Nada z)    = 0 + cantTesorosEntre (x - 1) (y - 1) z
-- if (x >= 0)      then 0 + cantTesorosEntre (x - 1) (y - 1) c 
--                  else if (y >= 0) then unoSi(hayTesoroEnObjetos z) + cantTesorosEntre x (y - 1) c 
--                  else cantTesorosEntre x (y - 1) c 


--2.|TIPOS DE ARBÓREOS|
    --2.1
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show
--Para hacer pruebas--
arbol :: Tree Int
arbol = NodeT 1 
            (NodeT 2 
                EmptyT 
                    (NodeT 3 EmptyT EmptyT)) 
            (NodeT 2 
                (NodeT 3 EmptyT 
                    (NodeT 4 
                        (NodeT 5 EmptyT EmptyT) 
                    EmptyT)) 
                EmptyT) 
------------------------------------------------------
sumarT :: Tree Int -> Int
sumarT EmptyT        = 0
sumarT (NodeT x y z) = x + sumarT y + sumarT z
------------------------------------------------------
sizeT :: Tree a -> Int
sizeT EmptyT        = 0
sizeT (NodeT _ x y) = 1 + sizeT x + sizeT y
------------------------------------------------------
mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT        = EmptyT
mapDobleT (NodeT x y z) = NodeT (x * 2) (mapDobleT y) (mapDobleT z)
------------------------------------------------------
perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT _ EmptyT        = False
perteneceT x (NodeT y z p) = (x == y) || perteneceT x z || perteneceT x p 
------------------------------------------------------
aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT _ EmptyT        = 0
aparicionesT x (NodeT y z p) = unoSi(x == y) + aparicionesT x z + aparicionesT x p
------------------------------------------------------
leaves :: Tree a -> [a]
leaves EmptyT        = []
leaves (NodeT x y z) = singularSi x (algunoEsHoja y z) ++ leaves y ++ leaves z 

algunoEsHoja :: Tree a -> Tree a -> Bool
algunoEsHoja EmptyT _      = True 
algunoEsHoja _      EmptyT = True
algunoEsHoja _      _      = False

singularSi :: a -> Bool -> [a]
singularSi a True  = a : []
singularSi _ False = []
------------------------------------------------------
heightT :: Tree a -> Int
heightT EmptyT        = 0
heightT (NodeT _ x y) = 1 + max (heightT x) (heightT y)
------------------------------------------------------
mirrorT :: Tree a -> Tree a
--Partiendo de que "y" esta a la izquierda y "z" a la derecha.
mirrorT EmptyT        = EmptyT
mirrorT (NodeT x y z) = NodeT x (mirrorT z) (mirrorT y)
------------------------------------------------------
toList :: Tree a -> [a]
toList EmptyT        = []
toList (NodeT x y z) = toList y ++ [x] ++ toList z
------------------------------------------------------
levelN :: Int -> Tree a -> [a]
levelN _ EmptyT        = []
levelN x (NodeT y z p) = if (x == 0) then [y] 
                                     else levelN (x - 1) z ++ levelN (x - 1) p 
------------------------------------------------------
-- listPerLevel :: Tree a -> [[a]]
-- listPerLevel EmptyT        = [EmptyT]
-- listPerLevel (NodeT x y z) = [x] : listPerLevel y : listPerLevel z
----------------------------------------------------
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT        = []
ramaMasLarga (NodeT x y z) = x : ramaMasLarga (primeroSi_SegundoSino (heightT y > heightT z) (y,z)) 

primeroSi_SegundoSino :: Bool -> (a,a) -> a
primeroSi_SegundoSino True  (x,_) = x 
primeroSi_SegundoSino False (_,y) = y

------------------------------------------------------
-- todosLosCaminos :: Tree a -> [[a]]
-- todosLosCaminos EmptyT        = []
-- todosLosCaminos (NodeT _ y z) = [y : (todosLosCaminos y)] ++ [z : (todosLosCaminos z) ] 
    --2.2
data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA

eval :: ExpA -> Int
eval (Valor x) = x
eval (Sum x y) = (eval x) + (eval y)
eval (Prod x y) = (eval x) * (eval y)
eval (Neg x) = -(eval x)
------------------------------------------------------
-- simplificar :: ExpA -> ExpA
-- simplificar (Valor x) = x
-- simplificar (Sum 0 y) = simplificar y
-- simplificar (Sum y 0) = simplificar y
-- simplificar 
