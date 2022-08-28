{-	
----------------------------------------------
||	PRÁCTICA N°1 |TIPOS ALGEBRAICOS|		||
||	Alumno: Matias Laime					||
||	Fecha De Inicio: 19/08/2022				||
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
maxDelPar (x, y) = if fst(x, y) > snd(x, y)
		   then fst(x, y)
		   else snd(x, y)
	--2
	{-
		sucesor (sumar 7 (maxDelPar(2, 1)))
		sumar (snd(divisionYResto 20 2)) 10
		fst(divisionYResto (sumar 10 90) (maxDelPar(10, 5)))
		snd(divisionYResto 10 100)
	-}

--3.|TIPOS ENUMERATIVOS|
	--1 a)
data Dir = Norte | Este | Sur | Oeste deriving Show
opuesto :: Dir 
opuesto = Norte
	--1 b)
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Sur Sur = True
iguales Este Este = True
iguales Oeste Oeste = True
iguales _ _ = False
	--1 c)
siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = Norte
	--2 a)
data DiaDeLaSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo deriving Show
primerYUltimoDia :: (DiaDeLaSemana, DiaDeLaSemana)
primerYUltimoDia = (Lunes, Domingo)
	--2 b)
empiezaConM :: DiaDeLaSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False
	--2 c)
vieneDespues :: DiaDeLaSemana -> DiaDeLaSemana -> Bool
vieneDespues Lunes Martes = True
vieneDespues Martes Miercoles = True
vieneDespues Miercoles Jueves = True
vieneDespues Jueves Viernes = True
vieneDespues Viernes Sabado = True
vieneDespues Sabado Domingo = True
vieneDespues Domingo Lunes = True
vieneDespues _ _ = False
	--2 d)
estaEnElMedio :: DiaDeLaSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True
	--3 a)
negar :: Bool -> Bool
negar True = False
negar False = True
	--3 b)
implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True
	--3 c)
yTambien :: Bool -> Bool -> Bool
yTambien True True = True
yTambien _ _ = False
	--3 d)
oBien :: Bool -> Bool -> Bool
oBien False False = False
oBien _ _ = True

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
crecer (P n e) = P n (e + 1)

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre x (P n e) = (P x e)

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (P n e) (P x y) = e > y

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor x y = if (esMayorQueLaOtra x y)
				   then x
				   else y

	--2
data TipoDePokemon = Agua | Electrico | Lucha | Tierra deriving Show 
data Pokemon = Pk TipoDePokemon Int deriving Show
							  --Porcentaje de energia del pokemon
data Entrenador = Ent String Pokemon Pokemon deriving Show
					--Nombre

pikachu = Pk Electrico 76
machoke = Pk Lucha 89
squirtle = Pk Agua 54
cubone = Pk Tierra 95
matu = Ent "Matias" pikachu cubone
leo = Ent "Leandro" squirtle machoke

superaA :: Pokemon -> Pokemon -> Bool
superaA (Pk t e) (Pk x y) = e > y

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int 
cantidadDePokemonDe t (Ent n p1 p2) = 
	if ((sonDelMismoTipoDePokemon (tipoDePokemonDe p1) t) && (sonDelMismoTipoDePokemon (tipoDePokemonDe p2) t))
		then 2
	else if ((sonDelMismoTipoDePokemon (tipoDePokemonDe p1) t) || (sonDelMismoTipoDePokemon (tipoDePokemonDe p2) t))
		then 1
	else 0

sonDelMismoTipoDePokemon :: TipoDePokemon -> TipoDePokemon -> Bool
sonDelMismoTipoDePokemon Agua Agua = True
sonDelMismoTipoDePokemon Lucha Lucha = True
sonDelMismoTipoDePokemon Tierra Tierra = True
sonDelMismoTipoDePokemon Electrico Electrico = True
sonDelMismoTipoDePokemon _ _ = False

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
estaVacia _ = False

elPrimero :: [a] -> a
elPrimero (x : _) = x

sinElPrimero :: [a] -> [a]
sinElPrimero (_ : x) = x 

splitHead :: [a] -> (a, [a])
splitHead a = (elPrimero a, sinElPrimero a)