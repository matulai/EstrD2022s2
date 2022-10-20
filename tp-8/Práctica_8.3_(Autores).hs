--Modo: Usuario
--a)
programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum
--Propósito: dadas dos personas y un organizador, denota el conjunto de aquellos programas en las que 
--las personas programaron juntas.
--Eficiencia: O(C log C + 2 log P) siendo P la cantidad de programas en los que participo p1  
programasEnComun p1 p2 org = intersection (programasDe p1 org) (programasDe p2 org)

esUnGranHacker :: Organizador -> Persona -> Bool
--Propósito: denota verdadero si la persona indicada aparece como autor de todos los programas del organizador.
--Eficiencia: O(C + log P ) Siendo C la cantidad de codigos del organizador y P la cantidad de personas.
esUnGranHacker org p = nroProgramasDePersona org p == length (todosLosProgramas org) 

{-
Modo: Implementador 
Representación:
    data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))

Invariante de representación:
    -No existen Checksum sin autores en el Map.
    -No existen Personas que no hayan participado en un Checksum.
    -Si en el Map Checksum-Persona una persona es autor de un Checksum entonces ese Checksum tambien
     debe estar en los Checksum en los que es autor esa persona en el Map Persona-Checksum y viceversa. 
    -Una Persona no puede estar como autor mas de una vez en algún Cheacksum.
    -
    -
-}


nuevo :: Organizador
--Propósito: Un organizador vacío.
--Eficiencia: O(1)
nuevo = MkO emptyM emptyM 

agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
--Propósito: Agrega al organizador un programa con el Checksum indicado; el conjunto es el conjunto de personas autores
--de dicho programa.
--Precondición: el identificador del programa que se agrega no fue usado previamente en el organizador, y el Set de personas
--no está vacío.
--Eficiencia: no hay ninguna garantía de eficiencia.
agregarPrograma (MkO mCP mPC) c sP = MkO (assoc c sP mCP) (asociarPersonasCon c (set2List sP) mPC)  

asociarPersonasCon :: Cheacksum -> [Persona] -> Map Persona Cheacksum -> Map Persona Cheacksum
asociarPersonasCon c []     mPC = mPC 
asociarPersonasCon c (p:ps) mPC = case lookupM p mPC
                                  Just cs -> assoc p (c:cs) (asociarPersonasCon c ps mPC) 
                                  Nothing -> assoc p c (asociarPersonasCon c ps mPC) 

todosLosProgramas :: Organizador -> [Checksum]
--Propósito: denota una lista con todos y cada uno de los códigos identificadores de programas del organizador.
--Eficiencia: O(C) en peor caso, donde C es la cantidad de códigos en el organizador.
todosLosProgramas (MkO mCP _) = domM mCP

autoresDe :: Organizador -> Checksum -> Set Persona
--Propósito: denota el conjunto de autores que aparecen en un programa determinado.
--Precondición: el Checksum debe corresponder a un programa del organizador.
--Eficiencia: O(log C) en peor caso, donde C es la cantidad total de programas del organizador.
autoresDe (MkO mCP _) c = fromJust (lookupM c mCP)


programasDe :: Organizador -> Persona -> Set Checksum
--Propósito: denota el conjunto de programas en los que participó una determinada persona.
--Precondición: la persona debe existir en el organizador.
--Eficiencia: O(log P) en peor caso, donde P es la cantidad total de personas del organizador.
programasDe (MkO _ mPC) p = fromJust (lookupM p mPC)

programaronJuntas :: Organizador -> Persona -> Persona -> Bool
--Propósito: dado un organizador y dos personas, denota verdadero si ambas son autores de algún software en común.
--Precondición: las personas deben ser distintas.
--Eficiencia: O(log P + C log C) en peor caso, donde P es la cantidad de personas distintas que aparecen en todos los
--programas del organizador, y C la cantidad total de programas.
programaronJuntas (MkO mCP mPC) p1 p2 = not (isEmptyS (intersection (programasDe p1) (programasDe p2)))  

nroProgramasDePersona :: Organizador -> Persona -> Int
--Propósito: dado un organizador y una persona, denota la cantidad de programas distintos en los que aparece.
--Eficiencia: O(log P) en peor caso, donde P es la cantidad de personas del organizador.
nroProgramasDePersona (MkO mCP mPC) p = case lookupM p mPC of
                                            Just cs -> length (set2List cs)
                                            Nothing -> 0

-- elMayorPrograma :: Organizador -> Maybe Checksum
-- --Propósito: recibe un organizador y denota uno de los programas con más autores de todo ese organizador; denota
-- --Nothing si no puede devolver un programa.
-- --Eficiencia: O(1) en peor caso.
-- --Esto puede requerir modificar el tipo de representación, agregar invariantes, y modificar operaciones existentes. Reescribir
-- --sólo las operaciones que tienen cambios sustanciales y no en las que, por ejemplo, sólo se modifica un pattern matching.
-- elMayorPrograma (MkO mCP mPC) = 

--data Organizador = MkO c (Map Checksum (Set Persona)) (Map Persona (Set Checksum)) 

autoresDe :: Organizador -> Checksum -> Set Persona
--Propósito: denota el conjunto de autores que aparecen en un programa determinado.
--Eficiencia: O(log C) en peor caso, donde C es la cantidad total de programas del organizador.
autoresDe (MkO mCP _) c = case lookupM c mCP
                                Just sP -> sP
                                Nothing -> emptyS
