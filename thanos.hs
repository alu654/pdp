-- personajes que tienen una edad, una energía, una serie de habilidades (como por ejemplo “usar espada”, “controlar la mente”, etc),
-- su nombre y en qué planeta viven.

import Text.Show.Functions()
import Data.Char
import Data.List (genericLength)

type Habilidades = String
type Universo = [Personaje]
type Poder = Personaje -> Personaje


data Personaje = Personaje{ 
edad                 :: Int,
energia              :: Int,
habilidad            :: [Habilidades],
nombre               :: String,
planeta              :: String
}deriving (Eq,Show)

data Guantelete = Guantelete{ 
material             :: String,
poderes              :: [Personaje -> Personaje],
cantidadDeGemas      :: Int
}deriving (Show)

ironMan :: Personaje
ironMan = Personaje 45 100 ["Correr" , "Saltar" ] "Iron Man " "Tierra"

guantelete :: Guantelete
guantelete = Guantelete "uru" [] 6

guanteleteCompleto :: Guantelete -> Bool
guanteleteCompleto unGuante = cantidadDeGemas unGuante == 6 && material unGuante == "uru"

chasquear :: Guantelete -> Universo  -> Universo
chasquear unGuante unUniverso
            | guanteleteCompleto unGuante =  take (cantidadDePersonas unUniverso) unUniverso
            | otherwise = unUniverso

cantidadDePersonas :: Universo -> Int
cantidadDePersonas  = round.(/2).genericLength 


--Punto 2: (3 puntos) Resolver utilizando únicamente orden superior.
--Saber si un universo es apto para péndex, que ocurre si alguno de los personajes que lo integran tienen menos de 45 años.

edadDeUnParticipante:: Personaje -> Bool
edadDeUnParticipante unPersonaje = edad unPersonaje < 45

esApto :: Universo -> Bool
esApto unUniverso = any edadDeUnParticipante unUniverso


--Saber la energía total de un universo que es la sumatoria de todas las energías 
--de sus integrantes que tienen más de una habilidad

sonVariasHabilidades :: Personaje -> Bool
sonVariasHabilidades =(>1).length.habilidad

filtrarSegunHabilidades :: Universo -> Universo
filtrarSegunHabilidades universo = filter sonVariasHabilidades universo

obtenerEnergias :: Universo -> [Int]
obtenerEnergias =  map energia.filtrarSegunHabilidades

energiaTotal :: Universo -> Int
energiaTotal = sum.obtenerEnergias

{-Segunda parte-}

--La mente que tiene la habilidad de debilitar la energía de un usuario en un valor dado.
 
laMente :: Int -> Personaje -> Personaje
laMente num unPersonaje = unPersonaje { energia = energia unPersonaje - num}

--El alma puede controlar el alma de nuestro oponente permitiéndole eliminar una habilidad en particular si es que la posee. 
--Además le quita 10 puntos de energía. 
 
eliminarHabilidad:: Habilidades -> Poder
eliminarHabilidad unaHabilidad unPersonaje = unPersonaje {habilidad = filter (/= unaHabilidad) (habilidad unPersonaje)}

elalma :: Habilidades -> Poder
elalma  unPersonaje = restar10.eliminarHabilidad unPersonaje

restar10:: Poder
restar10  unPersonaje = unPersonaje {energia = energia unPersonaje - 10}

--El espacio que permite transportar al rival al planeta x (el que usted decida) y resta 20 puntos de energía.

cambiarPlaneta :: String -> Poder
cambiarPlaneta unPlaneta unPersonaje= unPersonaje {planeta = unPlaneta}

elEspacio :: String -> Poder
elEspacio planeta  = restar10.cambiarPlaneta planeta

-- El poder deja sin energía al rival y si tiene 2 habilidades o menos se las quita 
--(en caso contrario no le saca ninguna habilidad).

sacarEnergia :: Poder
sacarEnergia unPersonaje = unPersonaje{energia = 0}


cantidadDeHabilidades :: Personaje ->Int
cantidadDeHabilidades unPersonaje = length (habilidad unPersonaje) 

elPoder :: 







