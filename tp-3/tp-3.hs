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
data Color = Azul | Rojo
data Celda = Bolita Color Celda | CeldaVacia
--Funciones Observadoras
colorDeCelda :: Celda -> Color
colorDeCelda (Bolita x _) = x

celda :: Celda -> Celda 
celda CeldaVacia   = CeldaVacia
celda (Bolita _ y) = y

------------------------------------------------------
nroBolitas :: Color -> Celda -> Int
nroBolitas _    CeldaVacia   = 0
nroBolitas x    (Bolita y z) =  unoSi(sonColoresIguales x y) + nroBolitas x z
------------------------------------------------------
sonColoresIguales :: Color -> Color -> Bool
sonColoresIguales Azul Azul = True
sonColoresIguales Rojo Rojo = True
sonColoresIguales _    _    = False
------------------------------------------------------
poner :: Color -> Celda -> Celda
poner x CeldaVacia = Bolita x CeldaVacia
poner x (Bolita y z) = Bolita y (poner x z)
------------------------------------------------------
sacar :: Color -> Celda -> Celda
sacar _   CeldaVacia = CeldaVacia
sacar x (Bolita y z) = if (sonColoresIguales x y) then z  else Bolita y (sacar x z)
------------------------------------------------------
ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 _ _ = CeldaVacia
ponerN x y z = ponerN (x - 1) y (poner y z) 
------------------------------------------------------
    --1.2
data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino
--Funciones Observadoras
objetos :: Camino -> [Objeto]
objetos (Cofre x _) = x

camino :: Camino -> Camino 
camino (Cofre _ x) = x
camino (Nada x) = x
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
pasosHastaTesoro (Cofre x y) = if (hayTesoroEnObjetos x) then 0 else 1 + pasosHastaTesoro y
------------------------------------------------------
hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn x y = if (hayTesoro y) then (pasosHastaTesoro y <= x) else False
------------------------------------------------------
alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros x Fin         = x == 0
alMenosNTesoros x (Nada y)    = True && alMenosNTesoros x y
alMenosNTesoros x (Cofre y z) = True && alMenosNTesoros (x - unoSi(hayTesoroEnObjetos y)) z
------------------------------------------------------
cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre _ 0 _           = 0
cantTesorosEntre _ _ Fin         = 0
cantTesorosEntre x y (Nada z)    = 0 + cantTesorosEntre x y z
cantTesorosEntre x y (Cofre z c) = if (x > 0) 
                                    then 0 + cantTesorosEntre (x - 1) (y - 1) c 
                                    else if (y > 0)
                                    then unoSi(hayTesoroEnObjetos z) + cantTesorosEntre x (y - 1) c 
                                    else cantTesorosEntre x (y - 1) c 
--2.|TIPOS DE ARBÓREOS|
    --2.1
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
------------------------------------------------------
sumarT :: Tree Int -> Int
sumarT EmptyT        = 0
sumarT (NodeT x y z) = x + sumarT y + sumarT z
------------------------------------------------------
sizeT :: Tree a -> Int
sizeT EmptyT        = 0
sizeT (NodeT x y z) = 1 + sizeT y + sizeT z
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
leaves (NodeT x y z) = x : leaves y ++ leaves z
------------------------------------------------------
-- heightT :: Tree a -> Int
-- heightT EmptyT = 0
-- heightT x = profundidad x
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
levelN x (NodeT y z p) = if (x == 0) then [y] else levelN (x - 1) z ++ levelN (x - 1) p 
------------------------------------------------------
-- listPerLevel :: Tree a -> [[a]]
-- listPerLevel EmptyT        = [[EmptyT]]
-- listPerLevel (NodeT x y z) = [x] : listPerLevel y : listPerLevel z
------------------------------------------------------
-- ramaMasLarga :: Tree a -> [a]
-- ramaMasLarga EmptyT        = []
-- ramaMasLarga (NodeT x y z) = if (profundidad y > z) 
--                               then x : ramaMasLarga y 
--                               else x : ramaMasLarga y
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
simplificar :: ExpA -> ExpA
simplificar (Valor x) = x
simplificar (Sum 0 y) = simplificar y
simplificar (Sum y 0) = simplificar y
simplificar 