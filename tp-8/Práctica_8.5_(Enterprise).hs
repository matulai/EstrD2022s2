--a)
tripulantes :: Nave -> Set Tripulante
--Propósito: Denota los tripulantes de la nave
tripulantes n = tripulantesN (sectores n) n

tripulantes :: [Sector] -> Nave -> Set Tripulante
tripulantes []     n = emptyS
tripulantes (s:ss) n = intersection (tripulantesDe s n) (tripulantes ss n)

bajaDeTripulante :: Tripulante -> Nave -> Nave
--Propósito: Elimina al tripulante de la nave.
--Pista: Considere reconstruir la nave sin ese tripulante.
bajaDeTripulante t n = 

--b)
{-
Modo: Implementador
Representación:
    data Nave = MkN (Map Sector (Set Tripulante)) (Heap Tripulante) (Sector, Int)

Invariante de representación:
    -El sector del Par existe en el Map Sector-SetTripulante 
    -No hay tripulantes con el mismo nombre en el Heap
    -
    -
    -
-}
naveVacia :: [Sector] -> Nave
--Propósito: Crea una nave con todos esos sectores sin tripulantes.
--Precondición: la lista de sectores no está vacía
--Costo: O(S log S) siendo S la cantidad de sectores de la lista.
naveVacia (s:ss) = MkN (agregarSectoresM (s:ss) emptyM) emptyH (s,0)
                         
agregarSectoresM :: [Sector] -> Map Sector (Set Tripulante) -> Map Sector (Set Tripulante)
agregarSectoresM []     mSsT = mSsT  
agregarSectoresM (s:ss) mSsT = assocM s emptyS (agregarSectoresM ss mSsT)

tripulantesDe :: Sector -> Nave -> Set Tripulante
--Propósito: Obtiene los tripulantes de un sector.
--Costo: O(log S) siendo S la cantidad de sectores.
tripulantesDe s (MkN mSsT hT (s,x)) = case lookup s mSsT of
                                        Just sT -> sT
                                        Nothing -> error "No existe ese sector" 

sectores :: Nave -> [Sector]
--Propósito: Denota los sectores de la nave
--Costo: O(S) siendo S la cantidad de sectores.
sectores (MkN mSsT hT (s,x)) = domM mSsT 

conMayorRango :: Nave -> Tripulante
--Propósito: Denota el tripulante con mayor rango.
--Precondición: la nave no está vacía.
--Costo: O(1).
conMayorRango (MkN mSsT hT (s,x)) = findMin hT

conMasTripulantes :: Nave -> Sector
--Propósito: Denota el sector de la nave con más tripulantes.
--Costo: O(1).
conMasTripulantes (MkN mSsT hT (s,x)) = s

conRango :: Rango -> Nave -> Set Tripulante
--Propósito: Denota el conjunto de tripulantes con dicho rango.
--Costo: O(P log P) siendo P la cantidad de tripulantes.
conRango r (MkN mSsT hT (s,x)) = tripulantesConRangoEn r hT

tripulantesConRangoEn :: Rango -> Heap Tripulante -> Set Tripulante
tripulantesConRangoEn r hT = if isEmptyH
                                then emptyS
                                else if rango (findMin hT) == r 
                                    then adds (findMin hT) (tripulantesConRangoEn r (deleteMin hT))
                                    else tripulantesConRangoEn r (deleteMin hT)

sectorDe :: Tripulante -> Nave -> Sector
--Propósito: Devuelve el sector en el que se encuentra un tripulante.
--Precondición: el tripulante pertenece a la nave.
--Costo: O(S log S log P) siendo S la cantidad de sectores y P la cantidad de tripulantes.
sectorDe t n = sectorDeT (sectores n) t n 

sectorDeT :: [Sector] -> Tripulante -> Nave -> Sector 
sectorDeT (s:ss) t (MkN mSsT hT (s,x)) = let sT = fromJust (lookup s mSsT)  
                                         if belongs t sT 
                                            then s 
                                            else sectorDeT ss t (MkN mSsT hT (s,x))


agregarTripulante :: Tripulante -> Sector -> Nave -> Nave
--Propósito: Agrega un tripulante a ese sector de la nave.
--Precondición: El sector está en la nave y el tripulante no.
--Costo: No hay datos (justifique su elección).
agregarTripulante t s (MkN mSsT hT (s1,x)) =  
            MkN ( assocM s t mSsT) (insertH t hT) (actualizarS mSsT s1 s x)

actualizarS :: Map Sector (Set Tripulante) -> Sector -> Sector -> Int -> (Sector, Int)
actualizarS mSsT s1 s x = let sT = fromJust (lookup s1 mSsT)
                               y = size sT
                        in if y > x
                                then (s1, y)
                                else (s, x)
