--dave = ("Dave", 100, ["tejer","escribirPoesia"], [ardilla, libroPedKing])
import Data.Char (toUpper)

data Barbaro = Barbaro{
nombre :: String,
fuerza :: Int,
habilidad :: [Habilidades],
objetos :: [Objeto]
}
type B = Barbaro -> Barbaro
type Habilidades = String
type Objeto = Barbaro -> Barbaro

dave :: Barbaro
dave =  Barbaro "Dave" 100 ["tejer","escribirPoesia"] [ardilla, libroPedKing]

jorge :: Barbaro
jorge = Barbaro "Jorge" 80 ["Aooo","Eooo","Iooo","Oooo"] [ardilla {--,libroPedKing--}]

faffy :: Barbaro
faffy = Barbaro "Faffy" 100 ["robar"] [ardilla {--,libroPedKing--}]



--Las espadas aumentan la fuerza de los bárbaros en 2 unidades por cada kilogramo de peso.

aumentarFuerza ::Int-> B
aumentarFuerza n unBarbaro = unBarbaro {fuerza = fuerza unBarbaro + n}

espada :: Int -> B
espada n = aumentarFuerza n

ardilla :: B
ardilla unBarbaro =  unBarbaro

--Los amuletosMisticos puerco-marranos otorgan una habilidad dada a un bárbaro.

agregarHabilidad :: Habilidades -> B
agregarHabilidad unahabilidad unBarbaro = unBarbaro{habilidad = unahabilidad : habilidad unBarbaro}

amuletosMisticos :: Habilidades -> B  
amuletosMisticos unahabilidad = agregarHabilidad unahabilidad

--Las varitasDefectuosas, añaden la habilidad de hacer magia, pero desaparecen todos los demás objetos del bárbaro.

varitasDefectuosas:: B
varitasDefectuosas unBarbaro = agregarHabilidad "Magia".borrarHAbilidad $ unBarbaro


borrarHAbilidad :: B
borrarHAbilidad unBarbaro = unBarbaro{objetos = [varitasDefectuosas]}

cuerda :: Objeto -> Objeto -> Barbaro -> Barbaro
cuerda unaFuncion otraFuncion unBarbaro = (unaFuncion.otraFuncion) unBarbaro

--PARTE 2

concatenarHa:: Barbaro -> String
concatenarHa unBarbaro =  (concat.habilidad) unBarbaro

pasasHabilidadesAMayuscula :: Barbaro -> Barbaro
pasasHabilidadesAMayuscula unBarbaro = unBarbaro {habilidad =[(map toUpper.concatenarHa) unBarbaro]}

--invasionDeSuciosDuendes: Un bárbaro sobrevive si sabe “Escribir Poesía Atroz”
type Aventura = Barbaro -> Bool

invasionDeSuciosDuendes:: B
invasionDeSuciosDuendes unBarbaro  = agregarHabilidad "Saber Escribir" unBarbaro

tienePulgares :: String -> Bool
tienePulgares "Faffy" = False
tienePulgares "Astro" = False
tienePulgares _ = True

--saqueo: El bárbaro debe tener la habilidad de robar y tener más de 80 de fuerza.

tieneHabilidad :: Habilidades -> Barbaro -> Bool
tieneHabilidad nombre unBarbaro = elem nombre (habilidad unBarbaro) 



saqueo :: Aventura
saqueo unBarbaro= tieneHabilidad "Robar" unBarbaro && feurzaBArbaro (80) unBarbaro

feurzaBArbaro ::Int-> Aventura
feurzaBArbaro n = (>n).fuerza

{-gritoDeGuerra: El bárbaro debe tener un poder de grito de guerra mayor o igual a la cantidad de letras de sus habilidades. 
Este poder es igual a 4 veces la cantidad de objetos del bárbaro.
-}

cantHabilidad:: Barbaro -> Int
cantHabilidad unBarbaro = length.habilidad $ unBarbaro

cantidadDeObjetos :: Barbaro -> Int
cantidadDeObjetos unBarbaro = (length.objetos) unBarbaro


obtenerGritoDeGuerra :: Barbaro -> Int 
obtenerGritoDeGuerra unBarbaro = ((4*).cantidadDeObjetos) unBarbaro

--caligrafia: El bárbaro tiene caligrafía perfecta (para el estándar barbárico de la época) si sus 
--habilidades contienen más de 3 vocales y comienzan con mayúscula.


contieneMasDe3Vocales :: Habilidad -> Bool
contieneMasDe3Vocales unaHabilidad = (length.filter esVocal) unaHabilidad > 3

empiezaConMayuscula :: Habilidades-> Bool
empiezaConMayuscula unaHabilidad = isUpper . head $ unaHabilidad

--como "Escribir Poesia Atroz" comienza con mayuscula, agregamos los casos de habilidades que comienzen con mayuscula vocal. 
esVocal :: Char -> Bool
esVocal unaLetra = elem unaLetra "aeiouAEIOU" 

caligrafia :: Aventura
caligrafia unBarbaro = all (\unaHabilidad -> contieneMasDe3Vocales unaHabilidad && empiezaConMayuscula unaHabilidad) . habilidades $ unBarbaro

eliminarHabilidades :: [Habilidades] -> [Habilidades]
eliminarHabilidadesRepetidas [] = []
eliminarHabilidades (cabeza : cola) = cabeza : eliminarHabilidades (filter(/=cabeza)cola )