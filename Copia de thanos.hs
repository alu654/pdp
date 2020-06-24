import Text.Show.Functions()
import Data.Char (toUpper)
type Gema= Personaje -> Personaje
type Habilidad = String
type Universo = [Personaje]

data Guantelete = Guantelete {
                material :: String,
                gemas :: [Gema]
} deriving (Show)

data Personaje = Personaje {
                nombre :: String,
                edad :: Int,
                energia :: Int,
                habilidades :: [Habilidad],
                planeta :: String 
} deriving (Show)

wolverine :: Personaje
wolverine = Personaje "Wolverine" 45 100 ["saltar"] "Cilindro de Avellaneda"

capitanAmerica :: Personaje
capitanAmerica = Personaje "capitanAmerica" 45 100 [] "Cilindro de Avellaneda"

hulk :: Personaje
hulk = Personaje "hulk" 45 100 [] "Cilindro de Avellaneda"

guanteleteThanos :: Guantelete
guanteleteThanos = Guantelete "uru" [] 

cantGemas :: Guantelete -> Bool
cantGemas unGuante = length(gemas unGuante) == 6

materialunGuante :: String ->Guantelete -> Bool
materialunGuante unMaterial unGuante = material unGuante== unMaterial

chasquear ::Guantelete  ->  Universo -> Universo
chasquear unGuante  unPersonaje
                    | cantGemas unGuante && materialunGuante "uru" unGuante = dividirUniverso unPersonaje
                    | otherwise = unPersonaje

dividirUniverso :: Universo -> Universo
dividirUniverso unPersonaje = take(div(length unPersonaje )2 ) unPersonaje


pendex:: Universo -> Bool
pendex unPersonaje = any (< 45) (map edad unPersonaje)

cantidaDeHabilidades ::Int -> Universo -> Universo
cantidaDeHabilidades unValor unPersonaje = filter ((>unValor).length.habilidades) unPersonaje

sumatoriadeenergia :: Universo -> Int
sumatoriadeenergia unPersonaje= (sum.(map energia).cantidaDeHabilidades 1) unPersonaje

--La mente que tiene la habilidad de debilitar la energía de un usuario en un valor dado.

mente :: Int -> Personaje -> Personaje
mente valor unUniverso = debilitarEnergia valor unUniverso


debilitarEnergia :: Int-> Personaje -> Personaje
debilitarEnergia n universo = universo{energia =  energia universo + n}

eliminarHabilidad:: Habilidad->  Personaje -> Personaje
eliminarHabilidad unahabilidad unPersonaje = unPersonaje{habilidades =  (filter(/= unahabilidad).habilidades) unPersonaje}

alma ::Habilidad-> Gema
alma unahabilidad unPersonaje = (eliminarHabilidad unahabilidad . debilitarEnergia (10)) unPersonaje


--El espacio que permite transportar al rival al planeta x (el que usted decida) y resta 20 puntos de energía.


cambiarPlaneta :: String -> Personaje -> Personaje
cambiarPlaneta unPlaneta unPersonaje = unPersonaje {planeta = unPlaneta}

espacio :: String -> Personaje -> Personaje
espacio planetapa =cambiarPlaneta planetapa 

quitarHabilidades :: Gema
quitarHabilidades unPersonaje= unPersonaje{habilidades = []} 

--El poder deja sin energía al rival y si tiene 2 habilidades o menos se las quita (en caso contrario no le saca ninguna habilidad).
elPoder :: Gema
elPoder unOponente 
                    | length (habilidades unOponente) < 2 = (quitarHabilidades . debilitarEnergia (- energia unOponente) ) unOponente
                    | otherwise = unOponente


elTiempo :: Personaje -> Personaje
elTiempo unPersonaje = dividirEdad . debilitarEnergia (-50) $ unPersonaje


dividirEdad :: Personaje -> Personaje
dividirEdad unPersonaje = unPersonaje{edad = div(edad unPersonaje)2}


laGema :: Gema -> Gema
laGema unaGema = (unaGema.unaGema) 


{-Punto 5: (2 puntos). No se puede utilizar recursividad. Generar la función utilizar  que dado una lista de gemas y
 un enemigo ejecuta el poder de cada una de las gemas que lo componen contra el personaje dado. Indicar cómo se produce el “efecto de lado” sobre la víctima.-}

guanteleDeGoma :: Guantelete
guanteleDeGoma = Guantelete "goma" [elTiempo, (alma "usar Mjolnir"), laGema (alma "programacion en Haskell")]

utilizar :: [Gema] -> Personaje -> Personaje
utilizar listaDeGemas unPersonaje = foldl1 (.) listaDeGemas $ unPersonaje

--gemaMasPoderosa que dado un guantelete y una persona obtiene la gema del infinito que produce la pérdida más grande de energía sobre la víctima. 
delInfinito :: Guantelete -> Personaje -> Gema
delInfinito unGuantelete unPersonaje = gemaMasPoderosa (gemas unGuantelete) unPersonaje

gemaMasPoderosa :: [Gema] -> Personaje -> Gema
gemaMasPoderosa unaGema _ = head unaGema
gemaMasPoderosa (primera: segunda : cola) unPersonaje
    | (energia.primera $ unPersonaje ) > (energia.segunda $ unPersonaje) = gemaMasPoderosa (primera:cola) unPersonaje
    | otherwise = gemaMasPoderosa (segunda:cola) unPersonaje



--Punto 7: (1 punto) Dada la función generadora de gemas y un guantelete de locos:
infinitasGemas :: Gema -> [Gema]
infinitasGemas gema = gema:(infinitasGemas gema)

guanteleteDeLocos :: Guantelete
guanteleteDeLocos = Guantelete "vesconite" (infinitasGemas tiempo)

--Y la función 
usoLasTresPrimerasGemas :: Guantelete -> Personaje -> Personaje
usoLasTresPrimerasGemas guantelete = (utilizar . take 3. gemas) guantelete


