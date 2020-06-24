import Text.Show.Functions ()

data Turista = Turista{
cansancio :: Int,
stress :: Int,
viajaSolo:: Bool,
idiomas :: [String]
}deriving (Show)

{-Sus niveles de cansancio y stress
Si está viajando solo
Los idiomas que habla
-}

-- Ir a la playa: si está viajando solo baja el cansancio en 5 unidades, si no baja el stress 1 unidad.

irAlaPlaya :: Excursiones
irAlaPlaya unTurista | viajaSolo unTurista = cambiarCansancio (cansancio unTurista - 5) $ unTurista
                     | otherwise = cambiarStress (stress unTurista - 1) $ unTurista

cambiarCansancio ::  Int -> Excursiones
cambiarCansancio n unTurista = unTurista {cansancio = n}

cambiarStress ::  Int -> Excursiones
cambiarStress n unTurista = unTurista {stress = n}

--Apreciar algún elemento del paisaje: reduce el stress en la cantidad de letras de lo que se aprecia. 

apreciar :: String -> Excursiones
apreciar apreciacion unTurista = cambiarStress(stress unTurista - cantidadDeApreciacion apreciacion ) $ unTurista

cantidadDeApreciacion :: String -> Int
cantidadDeApreciacion apreciacion  = length apreciacion


{-Salir a hablar un idioma específico: el turista termina aprendiendo dicho idioma y continúa el viaje acompañado-}

agregarIdioma :: String -> Excursiones
agregarIdioma unIdioma unTurista = unTurista {idiomas = unIdioma : idiomas unTurista}

viajaAcompañado :: Excursiones
viajaAcompañado unTurista= unTurista {viajaSolo = False} 

hablandounIdiomaNuevo :: String -> Excursiones
hablandounIdiomaNuevo unIdioma unTurista = agregarIdioma unIdioma . viajaAcompañado $ unTurista


{-Caminar ciertos minutos: aumenta el cansancio pero reduce el stress según la intensidad de la caminad, ambos en la misma cantidad. 
El nivel de intensidad se calcula en 1 unidad cada 4 minutos que se caminen.-}

caminarCiertosMin :: Int ->  Excursiones
caminarCiertosMin tiempo unTurista  = cambiarCansancio (cansancio unTurista - tiempoCaminado tiempo) . cambiarStress (stress unTurista - tiempoCaminado tiempo) $ unTurista

tiempoCaminado :: Int -> Int
tiempoCaminado tiempo = div(tiempo)4

{-Paseo en barco: depende de cómo esté la marea
si está fuerte, aumenta el stress en 6 unidades y el cansancio en 10.
si está moderada, no pasa nada.
si está tranquila, el turista camina 10’ por la cubierta, aprecia la vista del “mar”, y sale a hablar con los tripulantes alemanes.
-}


data Marea = Fuerte|Moderada|Tranquila deriving (Show)

paseoEnBarco :: Marea -> Excursiones
paseoEnBarco Fuerte    unTurista = cambiarStress (stress unTurista + 6 ) . cambiarCansancio (cansancio unTurista +10) $ unTurista
paseoEnBarco Moderada  unTurista = unTurista
paseoEnBarco Tranquila unTurista = caminarCiertosMin (10) .apreciar "Mar" . hablandounIdiomaNuevo "alemanes" $ unTurista

type Excursiones = Turista -> Turista

--3
ana:: Turista
ana   = Turista 0 21 False ["espanol"]
beto :: Turista
beto  = Turista 15 15 True ["aleman"]
cathy :: Turista
cathy = Turista 15 15 True ["aleman","catalan"]

-- Hacer que un turista haga una excursión. Al hacer una excursión, el turista además de sufrir los efectos propios de la excursión, reduce en un 10% su stress.


hacerExcursion :: Excursiones -> Turista -> Turista
hacerExcursion unaExcursion unTurista = cambiarStress (stress unTurista -1  - div ((stress unTurista )*10) 100).unaExcursion $ unTurista

deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

type Indice = Turista->Int 
{-Definir la función deltaExcursionSegun que a partir de un índice, un turista y una excursión determine cuánto varió dicho índice 
después de que el turista haya hecho la excursión. Llamamos índice a cualquier función que devuelva un número a partir de un turista.
-}

deltaExcursionSegun :: Indice -> Turista -> Excursiones -> Int
deltaExcursionSegun indiceInicial unTurista unaExcursion = (indiceInicial.hacerExcursion unaExcursion $ unTurista) - indiceInicial unTurista

excursionEducativa::Turista->Excursiones->Bool
excursionEducativa turista = (>0) . deltaExcursionSegun (length.idiomas) turista 

--Conocer las excursiones desestresantes para un turista. Estas son aquellas que le reducen al menos 3 unidades de stress al turista.
excursionDesestresante::Turista->Excursiones->Bool
excursionDesestresante turista  excur =  deltaExcursionSegun stress turista excur  <= (-3)

{-Completo: Comienza con una caminata de 20 minutos para apreciar una "cascada", luego se camina 40 minutos hasta una playa,
 y finaliza con una salida con gente local que habla "melmacquiano".
Lado B: Este tour consiste en ir al otro lado de la isla a hacer alguna excursión (de las existentes) que elija el turista. 
Primero se hace un paseo en barco por aguas tranquilas (cercanas a la costa) hasta la otra punta de la isla, luego realiza 
la excursión elegida y finalmente vuelve caminando hasta la otra punta, tardando 2 horas.
Isla Vecina: Se navega hacia una isla vecina para hacer una excursión. Esta excursión depende de cómo esté la marea al llegar a la otra 
isla: si está fuerte se aprecia un "lago", sino se va a una playa. En resumen, este tour implica hacer un paseo en barco hasta la isla vecina, luego llevar a cabo dicha excursión, y finalmente volver a hacer un paseo en barco de regreso. La marea es la misma en todo el camino.
-}
type Tour = [Excursiones]

-- Comienza con una caminata de 20 minutos para apreciar una "cascada", luego se camina 40 minutos hasta una playa, y finaliza
--con una salida con gente local que habla "melmacquiano".

completo :: Tour
completo  =  [caminarCiertosMin 20,apreciar "cascada",caminarCiertosMin 40, irAlaPlaya, hablandounIdiomaNuevo "melmacquiano"]

{-Lado B: Este tour consiste en ir al otro lado de la isla a hacer alguna excursión (de las existentes) que elija el turista. Primero se hace un paseo 
en barco por aguas tranquilas (cercanas a la costa) hasta la otra punta de la isla, luego realiza la excursión elegida
y finalmente vuelve caminando hasta la otra punta, tardando 2 horas.-}

ladob :: Marea ->  Excursiones -> Tour
ladob unaMarea unaExcursion = [unaExcursion, paseoEnBarco unaMarea, caminarCiertosMin 120]

{-Isla Vecina: Se navega hacia una isla vecina para hacer una excursión. Esta excursión depende de
 cómo esté la marea al llegar a la otra isla: si está fuerte se aprecia un "lago", sino se va a una playa. 
 En resumen, este tour implica hacer un paseo en barco hasta la isla vecina, luego llevar a cabo dicha excursión,
  y finalmente volver a hacer un paseo en barco de regreso. La marea es la misma en todo el camino.
-}

 
islaVecino :: Excursiones -> Marea -> Tour
islaVecino unaExcursion  unaMarea = [ paseoEnBarco unaMarea, unaExcursion , paseoEnBarco unaMarea ]

{-Hacer que un turista haga un tour. Esto implica, primero un aumento del stress en tantas unidades 
como cantidad de excursiones tenga el tour, y luego realizar las excursiones en orden.-}

cantidadDeEXcursiones :: Tour -> Int
cantidadDeEXcursiones xTours = length xTours

hacerunTour :: Turista -> Tour ->  Turista
hacerunTour unTurista losTours = foldl1 (flip (.)) losTours . cambiarStress (stress unTurista + cantidadDeEXcursiones losTours) $ unTurista 

{-Dado un conjunto de tours, saber si existe alguno que sea convincente para un turista. Esto significa que el tour tiene alguna excursión
 desestresante la cual, además, deja al turista acompañado luego de realizarla.-}

esConvincente :: [Tour] -> Turista -> Bool
esConvincente unosTours unTurista = length(filter(convincente unTurista) $ unosTours) > 0

convincente:: Turista -> Tour -> Bool
convincente unTurista unaExcursion = length (filter (excursionDesestresante  unTurista) unaExcursion) > 0  && (viajaSolo . hacerunTour unTurista) unaExcursion


{-Saber la efectividad de un tour para un conjunto de turistas. Esto se calcula como la sumatoria de la espiritualidad 
recibida de cada turista a quienes les resultó convincente el tour. 
La espiritualidad que recibe un turista es la suma de las pérdidas de stress y cansancio tras el tour.
-}

sumatoria :: Tour -> Turista -> Indice -> Int
sumatoria unTour unTurista  indice = sum.map ( deltaExcursionSegun  indice  unTurista)  $ unTour

espiritualidad :: Turista -> Tour -> Int 
espiritualidad unTurista unTour = sumatoria unTour unTurista stress + sumatoria unTour unTurista cansancio

efectividad :: Tour -> [Turista] -> Int
efectividad unTour unosTuristas = sum.map ((flip espiritualidad)unTour) . filter (flip convincente unTour) $ unosTuristas 

playasInfinitas :: Tour
playasInfinitas = repeat irAlaPlaya

