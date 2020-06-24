import Text.Show.Functions

data Jugador = Jugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Show)

type Palo = (Habilidad -> Tiro)
type Limitaciones = Tiro -> Bool
type Modificaciones = Tiro -> Tiro
type Obstaculo = Tiro -> Tiro

-- Jugadores de ejemplo
bart = Jugador "Bart" "Homero" (Habilidad 25 60)
todd = Jugador "Todd" "Ned" (Habilidad 15 80)
rafa = Jugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = Tiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Show)

type Puntos = Int



cambiarVelocidad :: Int -> Tiro -> Tiro
cambiarVelocidad n unTiro = unTiro{velocidad = n}

cambiarPrecision :: Int -> Tiro -> Tiro
cambiarPrecision n unTiro= unTiro{precision = n} 

cambiarAltura:: Int -> Tiro -> Tiro
cambiarAltura n unTiro= unTiro{altura = n} 


-- Funciones útiles
--1)

dameTiro v p a = Tiro {velocidad = v, precision = p, altura = a}

putter :: Palo
putter habilidad = dameTiro 10 (2 * precisionJugador habilidad) 0

madera :: Palo
madera habilidad = dameTiro 100 (precisionJugador habilidad `div` 2) 5

hierro :: Int -> Palo
hierro n habilidad = dameTiro (n * fuerzaJugador habilidad) (precisionJugador habilidad `div` n) (max 0 n-3)

palos :: [Palo]
palos = [putter, madera] ++ map hierro [1..10]

-- 2) Definir la función golpe que dados una persona y un palo, obtiene el tiro resultante de usar ese palo con las habilidades de la persona.

golpe :: Jugador -> Palo -> Tiro
golpe jugador tiro = tiro(habilidad jugador)


--3)
--  del tiro se duplica, la precisión pasa a ser 100 y la altura 0.

precisionMayor :: Int -> Tiro -> Bool
precisionMayor n unTiro=  precision unTiro > 90

--Una laguna es superada si la velocidad del tiro es mayor a 80 y tiene una altura de entre 1 y 5 metros. 
-- Luego de superar una laguna el tiro llega con la misma velocidad y precisión, pero una altura equivalente a la altura original 
--dividida por el largo de la laguna.


velocidaddeTiro :: Tiro -> Bool
velocidaddeTiro unTiro=  velocidad unTiro >80

alturaDelTiro ::  Tiro -> Bool
alturaDelTiro unTiro = altura unTiro> 1 && altura unTiro < 5

tunel:: Int -> Tiro-> Tiro
tunel  distancia unTiro
           | precisionMayor 90 unTiro == True = cambiarVelocidad(velocidad unTiro * 2) . cambiarPrecision (100) . cambiarAltura(0) $unTiro
           | velocidaddeTiro unTiro && alturaDelTiro unTiro == True = cambiarVelocidad (80). cambiarPrecision (precision unTiro) . cambiarAltura (div(altura unTiro)distancia) $ unTiro
           | precisionMayor 95 unTiro && velocidadentre unTiro == True = cambiarAltura (0).cambiarPrecision (0). cambiarVelocidad(0)$ unTiro
           | otherwise = unTiro


--Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo con una precisión mayor a 95. 
--Al superar el hoyo, el tiro se detiene, quedando con todos sus componentes en 0.

velocidadentre :: Tiro -> Bool
velocidadentre unTiro = velocidad unTiro > 5 && velocidad unTiro <20 



-- Definir palosUtiles que dada una persona y un obstáculo, permita determinar qué palos le sirven para superarlo.

palosUtiles 


{-

-- El putter genera un tiro con velocidad igual a 10, el doble de la precisión recibida y altura 0.

dameTiro v p a = Tiro {velocidad = v, precision = p, altura = a}

putter:: Int -> Palos
putter habilidad = unTiro 10 (2 * precisionJugador habilidad) 0

--La madera genera uno de velocidad igual a 100, altura igual a 5 y la mitad de la precisión.

madera :: Palos
madera unTiro = cambiarVelocidad (100) . cambiarAltura (5) . cambiarPrecision(div(precision unTiro)2) $ unTiro

--Los hierros, que varían del 1 al 10 (número al que denominaremos n), generan un tiro de velocidad igual a la fuerza multiplicada por n, 
--la precisión dividida por n y una altura de n-3 (con mínimo 0). Modelarlos de la forma más genérica posible.

hierros :: Int-> Habilidad -> Tiro-> Tiro
hierros n habilidad  unTiro = cambiarVelocidad(n) . cambiarVelocidad(n * fuerzaJugador habilidad) . cambiarPrecision(div(precision unTiro)n) . cambiarAltura(n-3) $ unTiro



--Definir la función golpe que dados una persona y un palo, obtiene el tiro resultante de usar ese palo con las habilidades de la persona.

golpeFun ::  Jugador -> Palos -> Tiro
golpeFun unJugador unPalo = unPalo (habilidad unJugador)

-}