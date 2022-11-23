module Nave (
 Nave,
 SectorId,
 construir,
 ingresarT,
 sectoresAsignados,
 datosDeSector,
 tripulantesN,
 agregarASector,
 asignarASector
)

where

import Tripulante
import Sector
import Set
import Map
import MaxHeap

type Rango = String
type Nombre = String
type SectorId = String


-- a)
data Nave = N 
  (Map SectorId Sector) 
  (Map Nombre Tripulante) 
  (MaxHeap Tripulante) deriving Show
{-
  Inv. Rep:
  - si un tripulante existe como valor en el segundo map entonces
    existe en la maxHeap con el mismo rango y los mismos sectores
  - para cualquier tripulante que este registrado en el map debe existir
    como clave en el primer map cada uno de sus sectoresId y en el valor asociado
    a dicha clave debe haber un sector que contiene al tripulante anterior
  - cada sector registrado como valor en el primer map debe tener asociada una clave
    en el segundo por cada uno de sus tripulante
  Observaciones:
  - 

-}

-- b)
-- Propósito: Construye una nave con sectores vacíos, en base a una lista de identificadores de sectores.
-- Eficiencia: O(S)
-- costo real O(S log S)
-- porque se hace recursion sobre la lista de sectores O(S)
-- y en cada instancia de la recursion se utiliza assocM que es log(S)
construir :: [SectorId] -> Nave
construir ss = N (armarS ss) emptyM emptyH

armarS [] = emptyM
armarS (s:ss) = assocM s (crearS s) (armarS ss)


-- c) 
-- Propósito: Incorpora un tripulante a la nave, sin asignarle un sector.
-- Eficiencia: O(log T)
-- Observacion: si hay un tripulante con el nombre proviste, se omite la operacion
-- tanto lookupM, assocM, insertH son log(n) sobre la cantidad
--   de tripulante de la nave
ingresarT :: Nombre -> Rango -> Nave -> Nave
ingresarT n r (N sm tm mh) = 
  case lookupM n tm of
    Just _ -> N sm tm mh
    Nothing -> let t = (crearT n r) in
      N sm (assocM n t tm) (insertH t mh)

-- ingresarT "Jose" "002" $ ingresarT "Juan" "001" $  construir ["sectorX"]

-- d) 
-- Propósito: Devuelve los sectores asignados a un tripulante.
-- Precondición: Existe un tripulante con dicho nombre.
-- Eficiencia: O(log T)
-- porque lookupM es log sobre cantidad de tripulantes
sectoresAsignados :: Nombre -> Nave -> Set SectorId
sectoresAsignados n (N sm tm mh) = 
  case lookupM n tm of
    Just x -> sectoresT x
    Nothing -> error ("no existe el tripulante " ++ n) 


-- e) 
-- Propósito: Dado un sector, devuelve los tripulantes y los componentes asignados a ese sector.
-- Precondición: Existe un sector con dicho id.
-- Eficiencia: O(log S)
-- porque lookupM es log sobre la cantidad total de sectores
datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
datosDeSector s (N sm tm mh) = 
  case lookupM s sm of
    Just x -> (tripulantesS x, componentesS x)
    Nothing -> error ("no existe el sector " ++ s) 

-- f) 
-- Propósito: Devuelve la lista de tripulantes ordenada por rango, de mayor a menor.
-- Eficiencia: O(log T)
-- Costo real O(T log T)
-- por el costo de hToList sobre todos los tripulantes de la nave
tripulantesN :: Nave -> [Tripulante]
tripulantesN (N sm tm mh) = hToList mh

-- O(n log n)
-- porque se recorren todos los elementos
-- y en cada instancia de la recursion se utilizan
-- funciones de costo log sobre la estructura complet
hToList :: Ord a => MaxHeap a -> [a]
hToList mh = 
  if isEmptyH mh
    then []
    else maxH mh: (hToList (deleteMaxH mh))

-- g) 
-- Propósito: Asigna una lista de componentes a un sector de la nave.
-- si el sector no existe se crea sin tripulantes
-- Eficiencia: O(C + log S), siendo C la cantidad de componentes dados.
-- O(log S) por buscarOCrear sobre todos los sectores
-- + O(n) donde n es el tamaño de la lista provista
-- + log S por assocM
-- => O(n + log S)
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector cs sid (N sm tm mh) = 
  let sector = buscarOCrear sid sm in
    N (assocM sid (agregarCs cs sector) sm) tm mh

-- O(c)
-- por la recursion sobre los componentes
-- y por ser agregarC constante
agregarCs [] sector = sector
agregarCs (x:xs) sector = agregarC x (agregarCs xs sector)

-- O(log n)
-- donde n es la cantidad de claves de sm
buscarOCrear sid sm =
  case lookupM sid sm of
    Just x -> x
    Nothing -> crearS sid

-- h) 
-- Propósito: Asigna un sector a un tripulante.
-- Nota: No importa si el tripulante ya tiene asignado dicho sector.
-- Precondición: El tripulante y el sector existen
-- Eficiencia: O(log S + log T + T log T)
asignarASector :: Nombre -> SectorId -> Nave -> Nave
asignarASector n sid (N sm tm mh) = 
  let (Just s) = (lookupM sid sm) 
      (Just t) = (lookupM n tm)
      s' = agregarT n s
      t' = asignarS sid t in 
        N (assocM sid s' sm) (assocM n t' tm) (reemplazarH t' mh)
       
-- precondicion x existe en mh
reemplazarH x mh = 
  if isEmptyH mh
    then error ("el elemento " ++ (show x) ++ " no existe")
    else if x == maxH mh
      then insertH x (deleteMaxH mh)
      else insertH (maxH mh) (reemplazarH x (deleteMaxH mh))



