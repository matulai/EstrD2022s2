{-
Modo: Implementador
Representación: 
    data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)
    data Sector = S SectorId [Componente] Set Nombre
    data Tripulante = T Nombre Rango Set SectorId
    type Nombre = String
    type Rango = String
    type SectorId = String
    data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
    data Barril = Comida | Oxigeno | Torpedo | Combustible

Invariante de Representación:
    -Dentro del map Nombre-Tripulante cada nombre debe ser el del tripulante asociado.  
    -Si un tripulante existe en el Map Nombre-Tripulante entoces existe en el MaxHeap tripulante y viceversa,
     con el mismo nombre y los mismos sectores asignados.
    -No puede haber tripulantes iguales dentro del MaxHeap.
    -No puede haber dos tripulantes iguales en un mismo sector pero si puede un tripulante estar asignado a varios sectores.
    -Un sector puede estar vacio.
    -Un tripulante puede no tener sectores asignados.
-}

construir :: [SectorId] -> Nave
--Propósito: Construye una nave con sectores vacíos, en base a una lista de identificadores de sectores.
--Eficiencia: O(S)
construir ids = N (agregarSectores ids) emptyM emptyH

agregarSectores :: [SectorId] -> Map SectorId Sector
agregarSectores []       = emptyM
agregarSectores (id:ids) = assoc id (crearS id) (agregarSectores ids)

ingresarT :: Nombre -> Rango -> Nave -> Nave
--Propósito: Incorpora un tripulante a la nave, sin asignarle un sector.
--Precondición: El nombre del tripulante dado no existe en la nave dada.
--Eficiencia: O(log T)
ingresarT nt r (N mId mN mhT) = let t = crearT nt r in  
                                    N mId (assoc nt t mN) (insertH t mhT)  

sectoresAsignados :: Nombre -> Nave -> Set SectorId
--Propósito: Devuelve los sectores asignados a un tripulante.
--Precondición: Existe un tripulante con dicho nombre.
--Eficiencia: O(log M)
sectoresAsignados n (N _ mN _) = let t = fromJust (lookupM n mN) in
                                    sectoresT t 

datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
--Propósito: Dado un sector, devuelve los tripulantes y los componentes asignados a ese sector.
--Precondición: Existe un sector con dicho id.
--Eficiencia: O(log S)
datosDeSector id (N mId _ _) = let s = fromJust (lookupM id mId) in
                                    (tripulantesS s, componentesS s)

tripulantesN :: Nave -> [Tripulante]
--Propósito: Devuelve la lista de tripulantes ordenada por rango, de mayor a menor.
--Eficiencia: O(log T)
tripulantesN (N _ _ mhT) = mhToList mhT

mhToList :: MaxHeap Tripulante -> [Tripulante]
mhToList mhT = if isEmptyH mhT
                    then []
                    else maxH mhT : mhToList (deleteMaxH mhT) 

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
--Propósito: Asigna una lista de componentes a un sector de la nave.
--Eficiencia: O(C + log S), siendo C la cantidad de componentes dados.
agregarASector cs id (N mId mN mhT) = case lookupM id mId of
                                            Just s  -> N (assocM id (agregarComponentes cs s) mId) mN mhT
                                            Nothing -> N (assocM id (agregarComponentes cs (crearS id)) mId) mN mhT

agregarComponentes :: [Componente] -> Sector -> Sector
--Eficiencia: O(C)
agregarComponentes []     s = s
agregarComponentes (c:cs) s = agregarC c (agregarComponentes cs s) 

asignarASector :: Nombre -> SectorId -> Nave -> Nave
--Propósito: Asigna un sector a un tripulante.
--Nota: No importa si el tripulante ya tiene asignado dicho sector.
--Precondición: El tripulante y el sector existen.
--Eficiencia: O(log S + log T + T log T)
asignarASector n id (N mId mN mhT) = let Just s = lookupM id mId
                                         Just t = lookupM n mN
                                         sn     = agregarT n s 
                                         tn     = asignarS id t in 
                                        N (assocM id sn mId) (assocM n tn mN) (asignarSectorATripulanteH tn mhT)

agregarSectorATripulanteH :: Ord Tripulante -> Tripulante -> MaxHeap Tripulante -> MaxHeap Tripulante
--Precondición: El tripulante y el sector existen.
agregarSectorATripulanteH tn mhT = if tn == maxH mhT
                                        then insertH tn (deleteMaxH mhT)
                                        else insertH (maxH mhT) (agregarSectorATripulanteH t tn (deleteMaxH mhT)) 

--Modo: Usuario

sectores :: Nave -> Set SectorId
--Propósito: Devuelve todos los sectores no vacíos (con tripulantes asignados).
--Eficiencia: O(T * (N log N))
sectores n = sectoresNoVacios (tripulantesN n)  

sectoresNoVacios :: [Tripulante] -> Set SectorId
--Eficiencia: O(T * (N log N)) siendo N la cantidad de sectores asignados a cada tripulante.
sectoresNoVacios []     = emptyS
sectoresNoVacios (t:ts) = unionS (sectoresT t) (sectoresNoVacios ts)

sinSectoresAsignados :: Nave -> [Tripulante]
--Propósito: Devuelve los tripulantes que no poseen sectores asignados.
--Eficiencia: O(T)
sinSectoresAsignados n = tripulantesSinLaburar (tripulantesN n)

tripulantesSinLaburar :: [Tripulante] -> [Tripulante]
--Eficiencia: O(T)
tripulantesSinLaburar []     = []
tripulantesSinLaburar (t:ts) = if    size (sectoresT t) == 0
                                    then t : tripulantesSinLaburar ts 
                                    else tripulantesSinLaburar ts

barriles :: Nave -> [Barril]
--Propósito: Devuelve todos los barriles de los sectores asignados de la nave.
barriles n = barrilesN (sectores n) n 

barrilesN :: [SectorId] -> Nave -> [Barril]
barrilesN []       _ = []
barrilesN (id:ids) n = barrilesDe (snd (datosDeSector id n)) ++ barrilesN ids n

barrilesDe :: [Componentes] -> [Barril]
barrilesDe []     = [] 
barrilesDe (c:cs) = case barrilesEn c of
                        Just bs -> bs ++ barrilesDe cs 
                        Nothing -> barrilesDe cs

barrilesEn :: Componente -> Maybe Barril
barrilesEn Almacen bs = Just bs
barrilesEn _          = Nothing

--Bonus
--Dar una posible representación para el tipo Sector, de manera de que se pueda cumplir con el orden dado para cada
--operación de la interfaz, pero sin implementarlas.

{-
data Sector = S SectorId [Componente] Set Nombre 
-}