import Text.Show.Functions()
import Data.Char

-- Se sabe que los bárbaros tienen nombre, fuerza, habilidades y objetos, que los ayudarán más adelante 
--en su lucha contra el mal. Por ejemplo: 
-- dave = ("Dave", 100, ["tejer","escribirPoesia"], [ardilla, libroPedKing])

type Habilidades = String
type ObjetosAUsar = Participante -> Participante
type Aventura = Participante -> Bool



data Participante = UnParticipante{ 
nombre               :: String,
fuerza               :: Int,
habilidad            :: [Habilidades],
objetos              :: [ObjetosAUsar]
}deriving (Show)

dave :: Participante
dave = UnParticipante "dave" 100 ["tejer","escribirPoesia"] [ardilla,libroPedKing] 
--Las espadas aumentan la fuerza de los bárbaros en 2 unidades por cada kilogramo de peso.

espadas :: Int -> Participante -> Participante
espadas peso unParticipante   = aumentarpeso (peso * 2) unParticipante

--agregarFuerza fuerzaAgregar barbaro= barbaro{ fuerza= fuerza barbaro + fuerzaAgregar}

aumentarpeso :: Int -> Participante -> Participante
aumentarpeso peso unParticipante = unParticipante {fuerza = fuerza unParticipante  + peso}

--Los amuletosMisticos puerco-marranos otorgan una habilidad dada a un bárbaro.

esPuercomarranos :: String ->  Bool
esPuercomarranos  amuletosMisticos = amuletosMisticos == "puerco-marranos" 

otorgarHabilidad :: String -> Participante  -> String -> Participante
otorgarHabilidad unaHabilidad unParticipante unamuleto 
    | (esPuercomarranos unamuleto) =  unParticipante {habilidad = unaHabilidad : habilidad unParticipante} 
    | otherwise = unParticipante

--Las varitasDefectuosas, añaden la habilidad de hacer magia, pero desaparecen todos los demás objetos del bárbaro.
esVaritasDefectuosas :: String -> Bool
esVaritasDefectuosas esUnaVarita = esUnaVarita == "Varitas Defectuosas"

habilidadHacerMagia :: Participante  -> String -> Participante
habilidadHacerMagia  unParticipante varita 
    | (esVaritasDefectuosas varita) =  unParticipante { habilidad = "Magia" : habilidad unParticipante , objetos = []} 
    | otherwise = unParticipante

--Una ardilla, que no hace nada.
ardilla :: Participante -> Participante
ardilla unParticipante = unParticipante

libroPedKing:: Participante -> Participante
libroPedKing unParticipante = unParticipante

--Una cuerda, que combina dos objetos distintos, obteniendo uno que realiza las transformaciones de los otros dos.
--Punto 2
--El megafono es un objeto que potencia al bárbaro, concatenando sus habilidades y poniéndolas en mayúsculas. 
megafono :: Participante -> Participante  
megafono unParticipante = unParticipante {habilidad = [concatenarhabilidad  unParticipante] }


concatenarhabilidad :: Participante -> String
concatenarhabilidad unParticipante = map toUpper (concat(habilidad unParticipante))

--Punto 3

--invasionDeSuciosDuendes: Un bárbaro sobrevive si sabe “Escribir Poesía Atroz”

sabeEscribir :: Aventura 
sabeEscribir unParticipante  =  elem "Escribir Poesía Atroz"  (habilidad unParticipante)


sobrevive :: Participante -> String
sobrevive unParticipante 
     |sabeEscribir unParticipante = "sobrevive"
     | otherwise = "No sobrevive"


-- cremalleraDelTiempo: Un bárbaro sobrevive si no tiene pulgares. Los bárbaros llamados Faffy y Astro no tienen pulgares, los demás sí. 
 
cremalleraDelTiempo :: Aventura
cremalleraDelTiempo unParticipante = (nombre unParticipante == "faffy") || nombre unParticipante == "astro"

sobrebvivecremalleraDelTiempo :: Participante -> String
sobrebvivecremalleraDelTiempo unParticipante 
     |cremalleraDelTiempo unParticipante = "sobrevive"
     | otherwise                         = "No sobrevive"

{-
ritualDeFechorias: Un bárbaro puede sobrevivir si pasa una o más pruebas como las siguientes: 
saqueo: El bárbaro debe tener la habilidad de robar y tener más de 80 de fuerza.
gritoDeGuerra: El bárbaro debe tener un poder de grito de guerra mayor o igual a la cantidad de letras de sus habilidades.
-- Este poder es igual a 4 veces la cantidad de objetos del bárbaro.

-}


saqueo ::Participante -> Bool
saqueo unParticipante = habilidadDeRobar unParticipante || masFuerza80 unParticipante

masFuerza80 :: Participante -> Bool
masFuerza80 unParticipante =  fuerza unParticipante > 80

habilidadDeRobar :: Participante -> Bool
habilidadDeRobar unParticipante = habilidad unParticipante == ["Robar"]

gritoDeGuerra:: Aventura
gritoDeGuerra unParticipante = (cantidadDeLetras unParticipante <= length(objetos unParticipante))

cantidadDeLetras :: Participante -> Int
cantidadDeLetras unParticipante = length(concatenarhabilidad unParticipante)


--caligrafia: El bárbaro tiene caligrafía perfecta (para el estándar barbárico de la época) 
--si sus habilidades contienen más de 3 vocales y comienzan con mayúscula. 

esMayuscula :: Char -> Bool
esMayuscula letra = any (==letra) ['A'..'Z']

esVocal :: Char -> Bool
esVocal letra = elem letra "aeiouAEIOU"

cantidadVocales :: String -> Int
cantidadVocales palabra = length (filter(esVocal) palabra)

habilidadesMasDeTresVocales :: Aventura
habilidadesMasDeTresVocales unParticipante= all ((>3).cantidadVocales) (habilidad unParticipante) 

habilidadesComienzanConMayuscula :: Aventura
habilidadesComienzanConMayuscula unParticipante= all (esMayuscula) (map (head) (habilidad unParticipante))

caligrafia :: Aventura
caligrafia unParticipante = (habilidadesMasDeTresVocales unParticipante) && (habilidadesComienzanConMayuscula unParticipante)

ritualDeFechorias:: Aventura
ritualDeFechorias unParticipante = saqueo unParticipante || gritoDeGuerra unParticipante || caligrafia unParticipante

--Definir la función sobrevivientes que tome una lista de bárbaros y una aventura, 
--y diga cuáles bárbaros la sobreviven (es decir, pasan todas las pruebas)


sobrevivientes ::  Aventura  -> [Participante] -> [Participante]
sobrevivientes esUnaAventura  listaDeParticipantes= filter esUnaAventura listaDeParticipantes


--4 
--A - Los bárbaros se marean cuando tienen varias habilidades iguales. Por todo esto, 
--nos piden desarrollar una función que elimine los elementos repetidos de una lista 
habilidadIgual :: (Eq a) =>[a] -> [a]
habilidadIgual  []= []
habilidadIgual  (cabeza : cola) = cabeza : habilidadIgual (filter ((/=)cabeza) cola)

juntoLasPalabras :: [ObjetosAUsar] -> ObjetosAUsar
juntoLasPalabras unObjeto = foldl1 (.) unObjeto

obtenerelDescendiente:: Participante -> Participante
obtenerelDescendiente unParticipante =  unParticipante {nombre = (nombre unParticipante) ++ "*", fuerza= (fuerza unParticipante), habilidad= habilidadIgual (habilidad unParticipante), objetos = (objetos unParticipante)}

descendientes :: Participante -> [Participante]
descendientes unParticipante = (juntoLasPalabras (objetos unParticipante)) (obtenerelDescendiente unParticipante) : descendientes (obtenerelDescendiente unParticipante)