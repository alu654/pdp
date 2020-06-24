import Text.Show.Functions()


{-Todo héroe tiene un nombre, pero por lo general nos referimos a ellos por su epíteto. Sin embargo, 
hay muchos héroes que no conocemos. Esto se debe a que todo héroe tiene un reconocimiento y aquellos con uno bajo 
no han logrado pasar a la historia, aunque agradecemos eternamente su coraje ante la adversidad. Para ayudarse en sus pesares 
del día a día, los héroes llevan consigo diversos artefactos, que tienen una rareza, indicativos de lo valiosos que son.-}

data Heroe = Heroe {
epiteto :: String,
reconocimiento :: Int,
inteligencia :: Int,
artefactos ::[Artefacto]
}deriving (Show)

type Artefacto = (String,Int)

xiphos :: Artefacto
xiphos = ("Xiphos", 50)

lanzaDelOlimpo :: Artefacto
lanzaDelOlimpo = ("Lanza del Olimpo", 100)

relampagoDeZeus :: Artefacto
relampagoDeZeus = ("Relampago de Zeus", 500)

pistola :: Artefacto
pistola = ("Pistola", 100)

-- a.	Si su reconocimiento es mayor a 1000, su epíteto pasa a ser "El mítico", y no obtiene ningún artefacto. ¿Qué artefacto podría desear tal espécimen?
-- b.	Si tiene un reconocimiento de al menos 500, su epíteto pasa a ser "El magnífico" y añade a sus artefactos la lanza del Olimpo (100 de rareza). 
--c.	Si tiene menos de 500, pero más de 100, su epíteto pasa a ser "Hoplita" y añade a sus artefactos una Xiphos (50 de rareza).

reconocimientoValorMayor :: Int -> Heroe -> Bool 
reconocimientoValorMayor n  =(>n).reconocimiento

reconocimientoValorMenor :: Int -> Heroe -> Bool 
reconocimientoValorMenor n  =(>n).reconocimiento

pasaLahistoria :: Heroe -> Artefacto -> Heroe
pasaLahistoria unHeroe  unArtefacto
                |reconocimientoValorMayor (1000)unHeroe = unHeroe{epiteto = "El Mitico"}
                |reconocimientoValorMenor (500) unHeroe = unHeroe{epiteto = "El Mitico"  , artefactos =[ ("la lanza del Olimpo",10)] }--artefactos=  "la lanza del Olimpo" : nombre unArtefacto ,  rareza = 100}
                |reconocimientoValorMayor (100) unHeroe && reconocimientoValorMenor (500) unHeroe =  unHeroe {epiteto = "Hoplita" , artefactos =  ("Xiphos" ,  150) : artefactos unHeroe }
                | otherwise = unHeroe



--	Encontrar un artefacto: el héroe gana tanto reconocimiento como rareza del artefacto, además de guardarlo entre los que lleva.

{-encontrar :: String -> Int -> Heroe -> Heroe
encontrar artefactoNombre rareza unHeroe = agregarArtefactos artefactoNombre rareza unHeroe 
-}

agregarArtefactos :: Artefacto-> Heroe -> Heroe
agregarArtefactos artefacto unHeroe = unHeroe{reconocimiento = sndArtefacto artefacto, artefactos = artefacto : artefactos unHeroe}


sndArtefacto :: Artefacto -> Int
sndArtefacto  = snd

{-•	Escalar el Olimpo: esta ardua tarea recompensa a quien la realice otorgándole 500 unidades de reconocimiento y triplica la rareza 
de todos sus artefactos, pero desecha todos aquellos que luego de triplicar 
su rareza no tengan un mínimo de 1000 unidades. Además, obtiene "El relámpago de Zeus" (un artefacto de 500 unidades de rareza).-}


escalarelOlimpo:: Heroe -> Heroe
escalarelOlimpo  unHeroe = agregarReconocimiento 500 unHeroe


agregarReconocimiento :: Int -> Heroe-> Heroe
agregarReconocimiento n unHeroe= unHeroe{reconocimiento = n}

trabajarLosArtefactos :: Heroe -> [Artefacto]
trabajarLosArtefactos heroe = relampagoDeZeus : (sacardeArt heroe)

sacardeArt:: Heroe-> [Artefacto]
sacardeArt unHeroe  = filter( cumpleConRareza 1000). map mapSec . artefactos $ unHeroe

cumpleConRareza :: Int -> Artefacto -> Bool
cumpleConRareza valor = (>= valor) . sndArtefacto

mapSec :: Artefacto-> Artefacto
mapSec (nombre,rareza) = (nombre, rareza *3)

-----------------------------
{-Ayudar a cruzar la calle: incluso en la antigua Grecia los adultos mayores necesitan ayuda para ello.
 Los héroes que realicen esta tarea obtiene el epíteto "Groso", donde la última 'o' se repite tantas veces como 
 cuadras haya ayudado a cruzar. Por ejemplo, ayudar a cruzar una cuadra es simplemente "Groso", pero ayudar a cruzar 5 cuadras es "Grosooooo".
 -}


ayudar :: Int -> Heroe -> Heroe
ayudar n unHeroe = unHeroe{epiteto = "Gros" ++repetirLetra n 'o' }

repetirLetra cantidad letra = replicate cantidad letra


{-•	Matar una bestia: Cada bestia tiene una debilidad (por ejemplo: que el héroe tenga cierto artefacto, o que su reconocimiento sea al menos de tanto). 
Si el héroe puede aprovechar esta debilidad, entonces obtiene el epíteto de "El asesino de <la bestia>". Caso contrario, huye despavorido, perdiendo -}

type Debilidad = Heroe -> Bool
data Bestia = Bestia {
nombre :: String,
debilidad :: Debilidad}


matarBestia :: Bestia -> Heroe -> Heroe
matarBestia bestia heroe | (debilidad bestia) heroe = heroe {nombre = " el asesino de " } 
                         | otherwise = saleCorriendo heroe
