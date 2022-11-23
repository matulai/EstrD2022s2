import EscuelaDeMagia
import Set
import Mago

-- j) 
-- Propósito: Retorna todos los hechizos aprendidos por los magos.
-- Eficiencia: O(M ∗ (log M + H log H))
hechizosAprendidos :: EscuelaDeMagia -> Set Hechizo
hechizosAprendidos edm = recolectar (magos edm) edm

recolectar [] edm = emptyS
recolectar (m:ms) edm = 
    unionS (hechizosDe m edm) (recolectar ms edm)

-- k) 
-- Propósito: Indica si existe un mago que sabe todos los hechizos enseñados por la escuela.
-- Eficiencia: O(log M)
hayUnExperto :: EscuelaDeMagia -> Bool
hayUnExperto edm = 
  if not (estaVacia edm)
    then let (elMasCapo, _) = egresarUno edm in
      leFaltanAprender (nombre elMasCapo) edm == 0
    else False

-- l) 
-- Propósito: Devuelve un par con la lista de magos que saben todos los hechizos dados por la escuela y la escuela sin dichos
-- magos.
-- Eficiencia: O(M log M)
egresarExpertos :: EscuelaDeMagia -> ([Mago], EscuelaDeMagia)
egresarExpertos edm = 
  if hayUnExperto edm 
    then let (experto, edm') = egresarUno edm in
      let (expertos, edm'') = egresarExpertos edm' in
        (experto:expertos, edm'')
    else
      ([], edm)

escuela1 = enseniar "banish" "jose" $  enseniar "hocuspocus" "jose"  $ registrar "jose" fundarEscuela
escuela2 = enseniar "command" "juan" $  enseniar "hocuspocus" "juan"  $ registrar "juan" escuela1
escuela3 = enseniar "banish" "juan" $  enseniar "hocuspocus" "juan"  $ registrar "juan" escuela1
escuela4 = enseniar "banish" "juan" escuela2