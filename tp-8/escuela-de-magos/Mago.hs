module Mago(
  Mago,
  Hechizo,
  Nombre,
  crearM,
  nombre,
  aprender,
  hechizos
)
where

import Set

type Hechizo = String
type Nombre = String

data Mago = M Nombre (Set Hechizo) deriving Show

instance Ord Mago where
  (M _ s1) `compare` (M _ s2) = (sizeS s1) `compare` (sizeS s2)
instance Eq Mago where
  (M s1 _) == (M s2 _) = s1 == s2

-- O(1)
crearM :: Nombre -> Mago 
crearM n = M n emptyS
-- O(1)
nombre :: Mago -> Nombre 
nombre (M n hs) = n
-- O(log H)
aprender :: Hechizo -> Mago -> Mago 
aprender h (M n hs) = M n (addS h hs)
-- O(1)
hechizos :: Mago -> Set Hechizo 
hechizos (M n h) = h