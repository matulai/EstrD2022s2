{-	
----------------------------------------------
||	PRÁCTICA N°1 |TIPOS ALGEBRAICOS|    	||
||	Alumno: Matias Laime		    		||
||	Fecha De Inicio: 19/08/2022	    		||
----------------------------------------------
-}
--2.|NÚMEROS ENTEROS|
	--1 a)
sucesor :: Int -> Int
sucesor x = x + 1
	--1 b)
sumar :: Int -> Int -> Int
sumar x y = x + y
	--1 c)
divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto x y = (div x y, mod x y)
	--1 e)
maxDelPar :: (Int, Int) -> Int
maxDelPar (x, y) = primeroSi_SegundoSino (x > y) (x, y)

primeroSi_SegundoSino :: Bool -> (a, a) -> a
primeroSi_SegundoSino True (x, y)  = x
primeroSi_SegundoSino False (x, y) = y
	--2
	{-
		sucesor (sumar 8 (maxDelPar(0, 1)))
		primeroSi_SegundoSino (sumar 1 1 >= 2) ((divisionYResto 20 2)), 10)
		maxDelPar (10,1)
		sumar (sucesor 4) 5
	-}

--3.|TIPOS ENUMERATIVOS|
	--1 a)
data Dir = Norte | Este | Sur | Oeste deriving Show
opuesto :: Dir -> Dir
opuesto Norte 	= Sur
opuesto Este 	= Oeste
opuesto Sur 	= Norte
opuesto Oeste 	= Este
	--1 b)
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur   Sur 	= True
iguales Este  Este  = True
iguales Oeste Oeste = True
iguales _     _ 	= False
	--1 c)
siguiente :: Dir -> Dir
siguiente Norte	= Este
siguiente Este  = Sur
siguiente Sur   = Oeste
--Si posee una precondición, que la dirección dada no sea Oeste
--Es una función Parcial porque si le damos como dato Oeste devuelve un error.
	--2 a)
data DiaDeLaSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving Show

primerYUltimoDia :: (DiaDeLaSemana, DiaDeLaSemana)
primerYUltimoDia = (Lunes, Domingo)
	--2 b)
empiezaConM :: DiaDeLaSemana -> Bool
empiezaConM Martes    	= True
empiezaConM Miercoles 	= True
empiezaConM _ 		  	= False
	--2 c)
vieneDespues :: DiaDeLaSemana -> DiaDeLaSemana -> Bool
vieneDespues a b = (numeroDeDiaEnSemana a) > (numeroDeDiaEnSemana b)

numeroDeDiaEnSemana :: DiaDeLaSemana -> Int
numeroDeDiaEnSemana Lunes		= 1
numeroDeDiaEnSemana Martes		= 2
numeroDeDiaEnSemana Miercoles	= 3
numeroDeDiaEnSemana Jueves		= 4
numeroDeDiaEnSemana Viernes		= 5
numeroDeDiaEnSemana Sabado		= 6
numeroDeDiaEnSemana Domingo		= 7

	--2 d)
estaEnElMedio :: DiaDeLaSemana -> Bool
estaEnElMedio Lunes 	= False
estaEnElMedio Domingo 	= False
estaEnElMedio _ 		= True
	--3 a)
negar :: Bool -> Bool
negar True 	= False
negar False = True
	--3 b)
implica :: Bool -> Bool -> Bool
implica True a 	 	= a
implica _ 	 _ 		= True
	--3 c)
yTambien :: Bool -> Bool -> Bool
yTambien True a 	= a
yTambien _ 	  _ 	= False
	--3 d)
oBien :: Bool -> Bool -> Bool
oBien False a 	= a
oBien _ 	_ 	= True

--4.|REGISTROS|
	--1
data Persona = P String Int deriving Show
	       --Nombre Edad
matias = P "Matias" 21
leandro = P "Leandro" 23

nombre :: Persona -> String
nombre (P n e) = n

edad :: Persona -> Int
edad (P n e) = e

crecer :: Persona  -> Persona 
crecer (P n e) = P n (sucesor e)

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre x (P n e) = P x e

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (P n e) (P x y) = e > y

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor x y = primeroSi_SegundoSino (esMayorQueLaOtra x y) (x, y)

	--2
data TipoDePokemon = Agua | Fuego | Planta deriving Show 
data Pokemon = Pk TipoDePokemon Int deriving Show
		              --Porcentaje de energia del pokemon
data Entrenador = Ent String Pokemon Pokemon deriving Show
		    --Nombre

charizard = Pk Fuego 77
squirtle = Pk Agua 54
bulbasaur = Pk Planta 95
matu = Ent "Matias" pikachu cubone
leo = Ent "Leandro" squirtle machoke

superaA :: Pokemon -> Pokemon -> Bool
superaA p1 p2 = esEficaz_Contra (tipoDePokemonDe p1) (tipoDePokemonDe p2)

esEficaz_Contra :: TipoDePokemon -> TipoDePokemon -> Bool
esEficaz_Contra Agua 	Fuego 	= True
esEficaz_Contra Fuego 	Planta 	= True
esEficaz_Contra Planta 	Agua 	= True
esEficaz_Contra _ 		_		= False

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int 
cantidadDePokemonDe t (Ent _ p1 p2) = unoSi (sonDelMismoTipoDePokemon (tipoDePokemonDe p1) t) +
				      unoSi (sonDelMismoTipoDePokemon (tipoDePokemonDe p2) t)
	
unoSi :: Bool -> Int
unoSi True 	= 1
unoSi False	= 0

sonDelMismoTipoDePokemon :: TipoDePokemon -> TipoDePokemon -> Bool
sonDelMismoTipoDePokemon Agua  	   Agua			= True
sonDelMismoTipoDePokemon Lucha 	   Lucha 		= True
sonDelMismoTipoDePokemon Tierra    Tierra 		= True
sonDelMismoTipoDePokemon Electrico Electrico 	= True
sonDelMismoTipoDePokemon _ 		   _		 	= False

tipoDePokemonDe :: Pokemon -> TipoDePokemon
tipoDePokemonDe (Pk t e) = t

juntarPokemon :: Entrenador -> Entrenador -> [Pokemon]
juntarPokemon (Ent n p1 p2) (Ent m p3 p4) = p1 : p2 : p3 : p4 : []

--5.|FUNCIONES POLIMÓRFICAS|
	--1 a)
loMismo :: a -> a
loMismo x = x
	--1 b)
siempreSiete :: a -> Int
siempreSiete x = 7
	--1 c)
swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

	--2
{-
	Estas funciones son polimórficas ya que no dependen del tipo de dato que reciban
	para funcionar correctamente. 
-}

--6.|PATTERN MATCHING SOBRE LISTAS|
	--2
estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _	 = False

elPrimero :: [a] -> a
elPrimero (x : _) = x

sinElPrimero :: [a] -> [a]
sinElPrimero (_ : x) = x 

splitHead :: [a] -> (a, [a])
splitHead a = (elPrimero a, sinElPrimero a)