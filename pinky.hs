import Text.Show.Functions()

--De un animal se sabe su coeficiente intelectual (un número), su especie (un string) y sus capacidades (strings). 
type Habilidades = String

data Pinky = Pinky {
nombre :: String,
intelectual :: Int,
especie :: String,
habilidad :: [Habilidades]
}deriving (Show)

rata :: Pinky
rata = Pinky "Rata" 10 "Elefante" ["Comer","Cagar","Culiar"]
--inteligenciaSuperior: Incrementa en n unidades su coeficiente intelectual
incrementoIntelectual :: Int -> Pinky -> Pinky
incrementoIntelectual n unAnimal = unAnimal{intelectual =  n }

inteligenciaSuperior :: Int -> Pinky -> Pinky
inteligenciaSuperior n unAnimal = incrementoIntelectual n unAnimal

--pinkificar: quitarle todas las habilidades que tenía

--pinkificar :: Pinky-> Pinky
--pinkificar unAnimal  = agregarHabilidad _ unAnimal

{-superpoderes:  le da habilidades nuevas	
En caso de ser un elefante: le da la habilidad “no tenerle miedo a los ratones”
En caso de ser un ratón con coeficiente intelectual mayor a 100: le agrega la habilidad de “hablar”. 
Si no, lo deja como está. 
-}
agregarHabilidad :: Habilidades -> Pinky -> Pinky
agregarHabilidad unaHabilidad unAnimal= unAnimal{habilidad = unaHabilidad : habilidad unAnimal  }

elefante:: Pinky  -> Pinky 
elefante unAnimal 
                    | nombreAnimal "Elefante" unAnimal = agregarHabilidad "no tenerle miedo a los ratones" unAnimal
                    | nombreAnimal "Raton" unAnimal && mayorintelecto 100 unAnimal = agregarHabilidad "hablar" unAnimal
                    | otherwise =unAnimal


nombreAnimal :: String -> Pinky -> Bool 
nombreAnimal nombreAnimal unAnimal=  nombre unAnimal == nombreAnimal

mayorintelecto:: Int -> Pinky-> Bool
mayorintelecto n = (>n).intelectual

{-antropomórfico: si tiene la habilidad de hablar y su coeficiente es mayor a 60.
noTanCuerdo: si tiene más de dos habilidades de hacer sonidos pinkiescos. Hacer una función pinkiesco, 
que significa que la habilidad empieza con “hacer”, y luego va seguido de una palabra "pinkiesca", es decir, 
con 4 letras o menos y al menos una vocal. Ejemplo:
-}

antropomórfico :: Pinky -> Bool
antropomórfico unAnimal= mayorintelecto(60) unAnimal && tieneLahabiliad "Hablar" unAnimal

tieneLahabiliad :: Habilidades ->  Pinky -> Bool 
tieneLahabiliad unaHabilidad unAnimal = elem unaHabilidad (habilidad unAnimal)

pinkiesco :: Habilidades -> Bool
pinkiesco unaHabilidad  = take 5 unaHabilidad == "Hacer" && esPinkiesca unaHabilidad

esPinkiesca :: Habilidades -> Bool
esPinkiesca unaHabilidad =  length( unaHabilidad) <=10 


utlimas:: Pinky -> Bool
utlimas unaHabilidad= last (habilidad unaHabilidad) == "AEIOUaeiou"


type Experimento = Pinky -> Bool

juntaHabilidades :: [Habilidades] -> String
juntaHabilidades  lista =  concat( lista )