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
pasosHastaTesoro Fin         = 0
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
cantTesorosEntre :: Int -> Int -> Camino -> Int
cantTesorosEntre x y z = cantidadDeTesorosEn (tomarCaminoHasta (y - x + 1) (tomarCaminoDesde x z))

cantidadDeTesorosEn :: Camino -> Int
cantidadDeTesorosEn Fin           = 0
cantidadDeTesorosEn (Nada x)      = 0 + cantidadDeTesorosEn x
cantidadDeTesorosEn (Cofre x y)   = unoSi (hayTesoroEnObjetos x) + cantidadDeTesorosEn y

tomarCaminoHasta :: Int -> Camino -> Camino
tomarCaminoHasta 0 _           = Fin
tomarCaminoHasta _ Fin         = Fin
tomarCaminoHasta x (Nada y)    = Nada (tomarCaminoHasta (x - 1) y)
tomarCaminoHasta x (Cofre y z) = Cofre y (tomarCaminoHasta (x - 1) z)

tomarCaminoDesde :: Int -> Camino -> Camino
tomarCaminoDesde 0 x           = x
tomarCaminoDesde _ Fin         = Fin
tomarCaminoDesde x (Nada y)    = tomarCaminoDesde (x - 1) y
tomarCaminoDesde x (Cofre y z) = tomarCaminoDesde (x - 1) z

--2.|TIPOS DE ARBÓREOS|
    --2.1
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show
--Para hacer pruebas--
arbol :: Tree Int
arbol = NodeT 1 
            (NodeT 2 
                (NodeT 4 EmptyT EmptyT) 
                (NodeT 5 EmptyT EmptyT)) 
            (NodeT 3 
                (NodeT 6 EmptyT 
                    (NodeT 7 
                        (NodeT 8 EmptyT EmptyT) 
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
levelN 0 (NodeT y z p) = y : [] 
levelN x (NodeT y z p) = levelN (x - 1) z ++ levelN (x - 1) p 
---------------------------------------------------------
listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT        = []
listPerLevel (NodeT x y z) = [x] : juntarNiveles (listPerLevel y) (listPerLevel z)

juntarNiveles :: [[a]] -> [[a]] -> [[a]]
juntarNiveles []       x        = x
juntarNiveles x       []        = x
juntarNiveles (x : xs) (y : ys) = (x ++ y) : juntarNiveles xs ys 
----------------------------------------------------
ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT        = []
ramaMasLarga (NodeT x y z) = x : ramaMasLarga (primeroSi_SegundoSino (heightT y > heightT z) (y,z)) 

primeroSi_SegundoSino :: Bool -> (a,a) -> a
primeroSi_SegundoSino True  (x,_) = x 
primeroSi_SegundoSino False (_,y) = y

------------------------------------------------------
-- todosLosCaminos :: Tree a -> [[a]]
-- todosLosCaminos EmptyT        = [[]]
-- todosLosCaminos (NodeT x y z) = agregarATodas x (todosLosCaminos y) 
--                              ++ agregarATodas x (todosLosCaminos z)
-- todosLosCaminos :: Tree a -> [[a]]
-- todosLosCaminos EmptyT        = []
-- todosLosCaminos (NodeT x y z) = [x] : agregarATodas x (todosLosCaminos y) 
--                               ++ agregarATodas x (todosLosCaminos z)

-- agregarATodas :: a -> [[a]] -> [[a]]
-- agregarATodas _ []       = []
-- agregarATodas x (y : ys) = (x : y) : agregarATodas x ys

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT        = []
todosLosCaminos (NodeT x t1 t2) = consATodosDosList x (todosLosCaminos t1) (todosLosCaminos t2)

consATodosDosList :: a -> [[a]] -> [[a]] -> [[a]]
consATodosDosList x []       []        = [x : []]
consATodosDosList x yss      []        = consATodos x yss 
consATodosDosList x []       zss       = consATodos x zss
-- consATodosDosList x (ys:yss) (zs: zss) = (x : ys) : (x : zs) : consATodosDosList x yss zss
consATodosDosList x y z = consATodos x y ++ consATodos x z

consATodos :: a -> [[a]] -> [[a]]
consATodos x []       = [] 
consATodos x (ys:yss) = (x : ys) : consATodos x yss
{-
                                    1
                              2-----------3
                         4--------5   ||-----6
                      ||---||  ||---||    ||----7
                                            ||-----8
-}
arbol1 :: Tree Int
arbol1 = NodeT 1 (NodeT 2 EmptyT EmptyT) (NodeT 3 EmptyT EmptyT)
{-
    [[1,2,4], [1,2,5], [1,3,6,7,8]]
    [[2,4], [2,5]]     [[3,6,7,8]]
    [[4]]   [[5]]      [[6,7,8]]
    [[]]    [[]]       [[7,8]]
                       [[8]]
                       [[]]
-}
    --2.2
data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA deriving Show

expresion = Sum (Prod (Neg (Valor 3)) (Sum (Valor 5) (Valor 6) ) ) (Sum (Valor 3) (Valor 4))
--           (-3 * (5 + 6)) + (3 + 4)
expresion1 = Sum (Prod (Neg (Valor 3)) (Sum (Valor 0) (Valor 6) ) ) (Sum (Valor 3) (Valor 0))
--           (-3 * (0 + 6)) + (3 + 0)
expresion2 = Sum (Valor 0) (Valor (-3))
--          0 + (-3)
expresion3 = Neg (Valor (-3))

eval :: ExpA -> Int
eval (Valor x) = x
eval (Sum x y) = (eval x) + (eval y)
eval (Prod x y) = (eval x) * (eval y)
eval (Neg x) = -(eval x)
------------------------------------------------------
simplificar :: ExpA -> ExpA
simplificar (Valor x)  = (Valor x)
simplificar (Sum x y ) = simplificarSum (simplificar x) (simplificar y)
simplificar (Prod x y) = simplificarProd (simplificar x) (simplificar y) 
simplificar (Neg x)    = simplificarNeg (simplificar x)

simplificarSum :: ExpA -> ExpA -> ExpA
simplificarSum (Valor 0) (Valor y) = Valor y
simplificarSum (Valor x) (Valor 0) = Valor x
simplificarSum x         y = Sum x y

simplificarProd :: ExpA -> ExpA -> ExpA
simplificarProd (Valor 0) (Valor y) = Valor 0 
simplificarProd (Valor 0) (Valor y) = Valor 0
simplificarProd x          y = Prod x y

simplificarNegSi :: Int -> Bool -> Int
simplificarNegSi x True = -(x)
simplificarNegSi x _    = x

simplificarNeg :: ExpA -> ExpA
simplificarNeg (Valor x) = Valor (simplificarNegSi x (x < 0))
simplificarNeg x         = Neg x     