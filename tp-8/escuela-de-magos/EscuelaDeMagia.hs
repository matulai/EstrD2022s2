module EscuelaDeMagia (
 EscuelaDeMagia,
 fundarEscuela,
 estaVacia,
 registrar,
 magos,
 hechizosDe,
 leFaltanAprender,
 egresarUno,
 enseniar
)

where

import Mago
import Set
import Map
import PriorityQueue

-- a)
data EscuelaDeMagia = EDM 
  (Set Hechizo) 
  (Map Nombre Mago) 
  (PriorityQueue Mago) deriving Show
{-
  Inv. Rep
  - si un mago aprendio un hechizo, 
    entonces ese hechizo esta en el set de la escuela
  - todos los magos que estan en el map estan en la PQ y viceversa
    que implica misma cantidad de claves en el map que de elementos en la PQ
  - en la PQ no hay 2 magos con el mismo nombre
  - cada clave del map tiene asociado un mago que tiene como nombre
    dicha clave

ejemplo invalido: EDM emptyS  (assocM "jose" (aprender "ocuspocus" (crearM "jose")) emptyM) $ insertPQ (aprender "ocuspocus" (crearM 
"jose")) emptyPQ
ejemplo valido : EDM (addS "ocuspocus" emptyS)  (assocM "jose" (aprender "ocuspocus" (crearM "jose")) emptyM) $ insertPQ (aprender "ocuspocus" (crearM 
"jose")) emptyPQ
-}

-- b)
-- Propósito: Devuelve una escuela vacía.
-- Eficiencia: O(1)
fundarEscuela :: EscuelaDeMagia
fundarEscuela = EDM emptyS emptyM emptyPQ

-- c) 
-- Propósito: Indica si la escuela está vacía.
-- Eficiencia: O(1)
estaVacia :: EscuelaDeMagia -> Bool
estaVacia (EDM s m pq) = isEmptyPQ pq

-- d) 
-- Propósito: Incorpora un mago a la escuela (si ya existe no hace nada).
-- Eficiencia: O(log M)
registrar :: Nombre -> EscuelaDeMagia -> EscuelaDeMagia
registrar n (EDM s m pq) = 
  case lookupM n m of
    Just x -> EDM s m pq
    Nothing -> let mago = crearM n in
      EDM s (assocM n mago m) (insertPQ mago pq)

-- registrar "jose" fundarEscuela

-- e) 
-- Propósito: Devuelve los nombres de los magos registrados en la escuela.
-- Eficiencia: O(M)
-- porque keys es lineal sobre la cantidad de claves
-- que en este caso es igual a la cantidad de magos por IR
magos :: EscuelaDeMagia -> [Nombre]
magos (EDM s m pq) = keys m

-- magos $ registrar "jose" fundarEscuel

-- f) 
-- Propósito: Devuelve los hechizos que conoce un mago dado.
-- Precondición: Existe un mago con dicho nombre.
-- Eficiencia: O(log M)
-- porque lookupM es log(n) sobre la cantidad de claves
-- que en este caso es igual a la cantidad de magos por IR
-- hechizos es constante
hechizosDe :: Nombre -> EscuelaDeMagia -> Set Hechizo
hechizosDe n (EDM s m pq) = 
  case lookupM n m of
    Just x -> hechizos x
    Nothing -> error "el mago no esta registrado"

-- g) 
-- Propósito: Dado un mago, indica la cantidad de hechizos que la escuela ha dado y él no sabe.
-- Precondición: Existe un mago con dicho nombre.
-- Eficiencia: O(log M)
-- porque lookupM es log(n) sobre la cantidad de claves
-- que en este caso es igual a la cantidad de magos por IR
-- hechizos es constante al igual que sizeS
leFaltanAprender :: Nombre -> EscuelaDeMagia -> Int
leFaltanAprender n (EDM s m pq) = 
  case lookupM n m of
    Just x -> sizeS s - sizeS (hechizos x)
    Nothing -> error ("el mago no esta registrado" ++ n)

-- h) 
-- Propósito: Devuelve el mago que más hechizos sabe y la escuela sin dicho mago.
-- Precondición: Hay al menos un mago.
-- Eficiencia: O(log M)
-- porque deleteM al igual que deleteMaxPQ son log(n) 
-- sobre la cantidad de claves del map o elementos de la PQ
-- que en este caso es igual a la cantidad de magos por IR
-- (log M)+(log M) => 2(log M) y el 2 es constante
-- maxPQ es constante al igual que nombre
egresarUno :: EscuelaDeMagia -> (Mago, EscuelaDeMagia)
egresarUno (EDM s m pq) = 
  let elMasCapo = maxPQ pq in
    (elMasCapo, EDM s (deleteM (nombre elMasCapo) m) (deleteMaxPQ pq))

-- i) 
-- Propósito: Enseña un hechizo a un mago existente, y si el hechizo no existe en la escuela es incorporado a la misma.
-- Nota: No importa si el mago ya conoce el hechizo dado.
-- Precondición: Existe un mago con dicho nombre.
-- Eficiencia: O(M log M + log H)
-- M log M  por reemplazarPQ sobre la cantidad de magos de la escuela
-- log H por addS sobre s que tiene todos los hechizos
-- assocM sobre m se desprecia porque 
--   (M log M) + (log M) => log M * (M + 1)
--   y la costante se desprecia
enseniar :: Hechizo -> Nombre -> EscuelaDeMagia -> EscuelaDeMagia
enseniar h n (EDM s m pq) = 
  case lookupM n m of
    Just x -> let mago = aprender h x in
      EDM (addS h s) (assocM n mago m) (reemplazarPQ mago pq)
    Nothing -> error ("el mago no esta registrado" ++ n)

-- precondicion m existe en pq
-- O (n log n)
-- porque se realiza recursion sobre todos los elementos de la PQ
-- y en cada instancia de la recursion se utilizan las funciones
-- deleteMaxPQ e insertPQ que son log n sobre la cantidad de elementos
reemplazarPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a
reemplazarPQ m pq = 
  if isEmptyPQ pq 
    then error "el elemento no existe"
    else if m == maxPQ pq 
      then insertPQ m (deleteMaxPQ pq)
      else insertPQ (maxPQ pq) $ reemplazarPQ m (deleteMaxPQ pq)