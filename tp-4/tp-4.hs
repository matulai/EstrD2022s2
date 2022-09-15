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
pizza = Capa Salsa (Capa (Aceitunas 5) (Capa (Aceitunas 6) (Capa Queso (Capa Salsa Prepizza ))))
--Funciones observadoras------------------------------
siguientePizza :: Pizza -> Pizza
siguientePizza (Capa _ p) = p
siguientePizza _          = Prepizza

ingrediente :: Pizza -> Ingrediente
ingrediente (Capa i _) = i
------------------------------------------------------
cantidadDeCapas :: Pizza -> Int 
cantidadDeCapas Prepizza   = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p

armarPizza :: [Ingrediente] -> Pizza
armarPizza []     = Prepizza
armarPizza (i:is) = Capa i (armarPizza is) 

sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa i p) = if (ingredientesIguales i Jamon) then sacarJamon p else Capa i (sacarJamon p)

ingredientesIguales :: Ingrediente -> Ingrediente -> Bool
ingredientesIguales Salsa         Salsa         = True
ingredientesIguales Queso         Queso         = True
ingredientesIguales Jamon         Jamon         = True
ingredientesIguales (Aceitunas _) (Aceitunas _) = True
ingredientesIguales _             _             = False 

tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza   = True 
tieneSoloSalsaYQueso (Capa i p) = (ingredientesIguales i Salsa || ingredientesIguales i Queso) 
                                && tieneSoloSalsaYQueso p

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza   = Prepizza
duplicarAceitunas p          = Capa (duplicarSiSonAceitunas (ingrediente p)) (duplicarAceitunas (siguientePizza p))

duplicarSiSonAceitunas :: Ingrediente -> Ingrediente
duplicarSiSonAceitunas (Aceitunas x) = Aceitunas (x * 2)
duplicarSiSonAceitunas i             = i