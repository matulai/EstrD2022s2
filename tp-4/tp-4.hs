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
duplicarAceitunas (Capa i p) = if (ingredientesIguales i (Aceitunas 0)) 
                                then Capa (Aceitunas ((cantidadDeAceitunas i)* 2)) (duplicarAceitunas p) 
                                else Capa i (duplicarAceitunas p) 

cantidadDeAceitunas :: Ingrediente -> Int 
cantidadDeAceitunas (Aceitunas x) = x