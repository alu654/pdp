import Text.Show.Functions


data Gimnasta = Gimnasta{
nombre :: String,
edad:: Float,
peso :: Float,
tonificacion :: Float
}deriving (Show)


data Rutina = Rutina {
nombrerut :: String,
duraciontot :: Tiempo,
ejercicios :: [Ejercicio]
} deriving (Show)

type Ejercicio = Gimnasta -> Gimnasta
type Tiempo = Float
type Velocidad = Float

pancho = Gimnasta "Francisco" 40.0 120.0 1.0
andres = Gimnasta "Andy" 22.0 80.0 6.0

relax :: Tiempo -> Gimnasta -> Gimnasta
relax minutos gimnasta = gimnasta

--Saber si alguien está saludable, lo cual se cumple si no está obeso y tiene una tonificación mayor
--a 5. Alguien es obeso si pesa más de 100 kilos.

saludable :: Gimnasta -> Bool
saludable unGimnasta = tonificacion unGimnasta > 5 &&  peso unGimnasta <100

esObeso:: Gimnasta -> Bool
esObeso unGimnasta = (>100) (peso unGimnasta)

cambiarPeso ::Float-> Gimnasta -> Gimnasta
cambiarPeso n unGimnasta = unGimnasta{peso = n}

cambiartonificacion :: Float-> Gimnasta -> Gimnasta
cambiartonificacion n unGimnasta = unGimnasta{tonificacion = n}

--Si la persona es obesa, baja 1 kilo cada 150 calorías quemadas.
--Si no es obesa pero tiene más de 30 años y las calorías quemadas son más de 200, baja siempre un kilo.
--En cualquier otro caso se baja la cantidad de calorías quemadas dividido por el producto entre el peso y la edad de la persona.
bajarCalorias :: Float -> Gimnasta -> Gimnasta
bajarCalorias calorias unGimnasta
                        | esObeso unGimnasta =  cambiarPeso(peso unGimnasta - calorias /150) $unGimnasta
                        | califi calorias unGimnasta =  cambiarPeso(peso unGimnasta - 1) $unGimnasta
                        | otherwise = cambiarPeso (peso unGimnasta - (calorias / ((peso unGimnasta) * (edad unGimnasta)))) unGimnasta



califi :: Float -> Gimnasta -> Bool
califi calorias unGimnasta = not (esObeso unGimnasta)  || edad unGimnasta > 30 && calorias >200 

-- 3

--La caminata es un ejercicio en cinta con velocidad constante de 5 km/h.
caminata :: Float ->  Gimnasta -> Gimnasta
caminata tiempo unGimnasta = cambiarPeso(peso unGimnasta + tiempo * 5) $unGimnasta

{-El entrenamiento en cinta arranca en 6 km/h y cada 5 minutos incrementa la
velocidad en 1 km/h, con lo cual la velocidad máxima dependerá de los minutos-}

arranca :: Float -> Gimnasta -> Gimnasta
arranca tiempo unGimnasta 
                | tiempo < 5 =  cambiarPeso(peso unGimnasta -  6* 5) $unGimnasta
                |otherwise  = cambiarPeso(peso unGimnasta - tiempo * 6) $unGimnasta

-- 3b

pesas :: Float-> Int -> Gimnasta -> Gimnasta
pesas peso tiempo gimnasta
  | tiempo > 10 = cambiartonificacion (0.1 * peso  ) gimnasta
  | otherwise = gimnasta

  --La colina quema 2 calorías por minuto multiplicado por la inclinación de la colina.
colina :: Float -> Gimnasta -> Gimnasta
colina graduacion unGimnasta = cambiarPeso ( peso unGimnasta *(2*graduacion)) unGimnasta


