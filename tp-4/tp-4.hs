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
--1.|MAPA DE TESOROS (con bifurcación)|
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
-- caminoAlTesoro :: Mapa -> [Dir]
-- --PRECONDICIÓN: Debe existir un tesoro y es único.
-- caminoAlTesoro (Fin c)               = 
-- caminoAlTesoro (Bifurcacion c m1 m2) = 
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