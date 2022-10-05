module PERSONA
    (Persona, crearPersona, nombre, apellido, edad, crecerVeces)
where

data Persona  = P String Int 

{-
    INVARIANTE DE REPRESENTACIÓN: En P Nombre Apellido Int
        -Int no debe ser negativo por que representa la edad.
        -Siempre tiene que tener nombre y apellido.
        -Los nombres y apellidos deben estar separados por un espacio.
    CASOS VALIDOS:
        -"Matias Laime" 27
        -"Matias Laime"
        -"Matias Laime"
    CASOS INVALIDOS:
        -"Matias Laime" -27
        -"Matias" o "Laime" 
        -"MatiasLaime"
-}

crearPersona :: String -> Persona
crearPersona s = P s 0

nombre :: Persona -> String
nombre (P s _) = nombreP s

nombreP :: String -> String
nombreP []     = error "No pusiste la separación o el nombre"
nombreP (l:ls) = if [l] == " "
                    then "" 
                    else l : nombreP ls

apellido :: Persona -> String
apellido (P s _) = apellidoP s

apellidoP :: String -> String
apellidoP []     = error "No pusiste la separación o el apellido"
apellidoP (l:ls) = if [l] == " "
                    then ls 
                    else apellidoP ls

edad :: Persona -> Int
edad (P _ e) = e

crecerVeces :: Int -> Persona -> Persona
crecerVeces 0 p = p 
crecerVeces x p = crecerVeces (x - 1) (crecer p)

crecer :: Persona -> Persona
crecer (P s e) = P s (e + 1)
