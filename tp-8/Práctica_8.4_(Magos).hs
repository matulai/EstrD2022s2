{-
Modo: Implementador
Representación: 
    data EscuelaDeMagia = EDM (Set Hechizo) (Map Nombre Mago) (PriorityQueue Mago)
    data Mago = M Nombre Set Hechizo
    type Hechizo = String
    type Nombre = String

Invariante de Representación:
    -Los Hechizos que sepan los magos deben estar dentro de los que se enseñan en la escuela.
    -En el Map Nombre-Mago la clave debe ser el nombre del Mago. 
    -Si un Mago existe en la PriorityQueue entonces existe en el Map con las mismas caracteristicas y viceversa. 
    -Si el Map Nombre-Mago es vacio entonces la PQ tambien es Vacia y viceversa.
    -En la PQ no hay dos magos con el mismo nombre.
-}

fundarEscuela :: EscuelaDeMagia
--Propósito: Devuelve una escuela vacía.
--Eficiencia: O(1)
fundarEscuela = EDM emptyS emptyM emptyPQ

estaVacia :: EscuelaDeMagia -> Bool
--Propósito: Indica si la escuela está vacía.
--Eficiencia: O(1)
estaVacia (EDM _ _ pqM) = isEmptyPQ pqM  

registrar :: Nombre -> EscuelaDeMagia -> EscuelaDeMagia
--Propósito: Incorpora un mago a la escuela (si ya existe no hace nada).
--Eficiencia: O(log M)
registrar n (EDM sM mNM pqM) = case lookup n mNM of
                                    Just m  -> EDM sM mNM pqM
                                    Nothing ->  let m = crearM n in 
                                                EDM sM (assoc n m mNM) (insertPQ m pqM) 

magos :: EscuelaDeMagia -> [Nombre]
--Propósito: Devuelve los nombres de los magos registrados en la escuela.
--Eficiencia: O(M)
magos (EDM sM mNM pqM) = domM mNM 

hechizosDe :: Nombre -> EscuelaDeMagia -> Set Hechizo
--Propósito: Devuelve los hechizos que conoce un mago dado.
--Precondición: Existe un mago con dicho nombre.
--Eficiencia: O(log M)
hechizosDe n (EDM sM mNM pqM) = let m = fromJust (lookup n mNM) in
                                hechizos m

leFaltanAprender :: Nombre -> EscuelaDeMagia -> Int
--Propósito: Dado un mago, indica la cantidad de hechizos que la escuela ha dado y él no sabe.
--Precondición: Existe un mago con dicho nombre.
--Eficiencia: O(log M)
leFaltanAprender n (EDM sM mNM pqM) = sizeS sM - sizeS (hechizosDe n) 

egresarUno :: EscuelaDeMagia -> (Mago, EscuelaDeMagia)
--Propósito: Devuelve el mago que más hechizos sabe y la escuela sin dicho mago.
--Precondición: Hay al menos un mago.
--Eficiencia: O(log M)
egresarUno (EDM sM mNM pqM) =   let m = maxPQ pqM in
                                (m, EDM sM (deleteM (nombre m) mNM) (deleteMaxPQ pqM))

enseñar :: Hechizo -> Nombre -> EscuelaDeMagia -> EscuelaDeMagia
--Propósito: Enseña un hechizo a un mago existente, y si el hechizo no existe en la escuela es incorporado a la misma.
--Nota: No importa si el mago ya conoce el hechizo dado.
--Precondición: Existe un mago con dicho nombre.
--Eficiencia: O(M log M + log H)
enseñar h n (EDM sM mNM pqM) =  let m = aprender h (fromJust (lookup n mNM)) in
                                EDM (addS h sM) (assoc n  mNM) (enseñarHechizoAPQ m pqM)

enseñarHechizoAPQ :: Mago -> PriorityQueue Mago -> PriorityQueue Mago
enseñarHechizoAPQ m pqM = if m == maxPQ pqM
                              then insertPQ m (deleteMaxPQ pqM)
                              else insertPQ (maxPQ pqM) (enseñarHechizoAPQ m (deleteMaxPQ pqM)) 

--Modo: Usuario

hechizosAprendidos :: EscuelaDeMagia -> Set Hechizo
--Propósito: Retorna todos los hechizos aprendidos por los magos.
--Eficiencia: O(M ∗ (log M + H log H))
hechizosAprendidos edm = hechizosAprendidosEDM (magos edm) edm 

hechizosAprendidosEDM :: [Nombre] -> EscuelaDeMagia -> Set Hechizo
hechizosAprendidosEDM []     edm = emptyS
hechizosAprendidosEDM (n:ns) edm = unionS (hechizosDe n edm) (hechizosAprendidosEDM ns edm)

hayUnExperto :: EscuelaDeMagia -> Bool
--Propósito: Indica si existe un mago que sabe todos los hechizos enseñados por la escuela.
--Eficiencia: O(log M)
hayUnExperto edm = let n = nombre (fst (egresarUno edm)) in
                       leFaltanAprender n edm == 0

egresarExpertos :: EscuelaDeMagia -> ([Mago], EscuelaDeMagia)
--Propósito: Devuelve un par con la lista de magos que saben todos los hechizos dados por la escuela y la escuela sin dichos
--magos.
--Eficiencia: O(M log M)
--Bonus
--Dar una posible representación para el tipo Mago, de manera de que se pueda cumplir con el orden dado para cada operación
--de la interfaz, pero sin implementarlas.
egresarExpertos edm = if hayUnExperto edm 
                            then let (m, escuelaSinM) = egresarUno edm
                                     (ms, escuelaSinMs) = egresarExpertos escuelaSinM
                                 in (m:ms, escuelaSinMs)
                            else ([], edm)