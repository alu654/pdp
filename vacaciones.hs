import Text.Show.Functions


{--	Sus niveles de cansancio y stress
-	Si está viajando solo
-	Los idiomas que habla
-}
data Turista = Turista{
cansancio :: Int,
stress :: Int,
viajaSolo :: Bool,
idiomas :: [String]
}deriving (Show)

{-a.	Ana: está acompañada, sin cansancio, tiene 21 de stress y habla español.
b.	Beto y Cathi, que hablan alemán, viajan solos, y Cathi además habla catalán. Ambos tienen 15 unidades de cansancio y stress.
-}

ana :: Turista
ana = Turista 0 21 False ["Español"]

beto :: Turista
beto = Turista 15 15 True ["Alemán"]

cathi :: Turista
cathi = Turista 15 15 True ["Alemán", "Catalan"]

type Excursiones = Turista -> Turista
--Ir a la playa: si está viajando solo baja el cansancio en 5 unidades, si no baja el stress 1 unidad.
irAlaPlaya :: Excursiones
irAlaPlaya unTurista | viajaSolo unTurista == True = cambiarCansancio(cansancio unTurista - 5) unTurista
                     | otherwise = cambiarCansancio(cansancio unTurista - 1) unTurista


cambiarStress ::Int ->  Excursiones
cambiarStress n unTurista = unTurista{stress = n}

cambiarCansancio::Int ->  Excursiones
cambiarCansancio n unTurista = unTurista{cansancio = n}

cambiarAcompañamiento ::  Excursiones
cambiarAcompañamiento  unTurista = unTurista{viajaSolo = False}

-- Apreciar algún elemento del paisaje: reduce el stress en la cantidad de letras de lo que se aprecia

contarLetras :: String -> Int
contarLetras paisaje = length paisaje

apreciarPaisaje :: String -> Excursiones
apreciarPaisaje paisaje unTurista = cambiarStress (stress unTurista - contarLetras paisaje) unTurista

--Salir a hablar un idioma específico: el turista termina aprendiendo dicho idioma y continúa el viaje acompañado.

agregarIdioma :: String-> Excursiones
agregarIdioma idiomaAAprender unTurista= unTurista {idiomas= idiomaAAprender : idiomas unTurista}

idiomaEspecífico :: String -> Excursiones
idiomaEspecífico idiomaAprendido unTurista = agregarIdioma idiomaAprendido . cambiarAcompañamiento $ unTurista

{--	Caminar ciertos minutos: aumenta el cansancio pero reduce el stress según la intensidad de la caminad, ambos
 en la misma cantidad. El nivel de intensidad se calcula en 1 unidad cada 4 minutos que se caminen.-}

intensidad:: Int -> Int
intensidad tiempo = div(tiempo) 4 

caminarCadaCiertosMinutos :: Int-> Excursiones
caminarCadaCiertosMinutos tiempo unTurista = cambiarCansancio (cansancio unTurista + intensidad tiempo). cambiarStress (stress unTurista- intensidad tiempo) $ unTurista

{--	Paseo en barco: depende de cómo esté la marea
-	si está fuerte, aumenta el stress en 6 unidades y el cansancio en 10.
-	si está moderada, no pasa nada.
-	si está tranquila, el turista camina 10’ por la cubierta, aprecia la vista del “mar”, y sale a hablar con los tripulantes alemanes.
-}
type Marea = String

paseoEnBarco :: Marea -> Excursiones
paseoEnBarco  marea unTurista
                             | estaFuerte marea == True  = cambiarStress (stress unTurista + 6) . cambiarCansancio (cansancio unTurista  + 10) $ unTurista
                             | estamoderada marea == True = unTurista
                             | estatranquila marea == True =  caminarCadaCiertosMinutos 10. apreciarPaisaje ("Mar") $ unTurista
                             | otherwise = unTurista


estaFuerte :: String -> Bool
estaFuerte marea =  marea == "Esta Fuerte"  


estamoderada :: String -> Bool
estamoderada marea = marea == "Esta moderada"

estatranquila:: String -> Bool
estatranquila marea =  marea == "Esta Tranquila"

--Hacer que un turista haga una excursión. Al hacer una excursión, el turista además de sufrir los efectos propios de la excursión, reduce en un 10%  su stress.

hacerExcursión :: Excursiones ->Turista->Turista
hacerExcursión  excursion unTurista = disminuirEn10 (10) . excursion $ unTurista

disminuirEn10 :: Int-> Excursiones
disminuirEn10 n unTurista= unTurista {stress = stress unTurista * n}


type Indice = Turista->Int


deltaExcursionSegun::Indice->Turista->Excursiones->Int
deltaExcursionSegun indice turista excursion = disminuirEn (indice turista)  . indice . hacerExcursión excursion $ turista

excursionEducativa::Turista->Excursiones->Bool
excursionEducativa turista = (>0) . deltaExcursionSegun (length.idiomas) turista 

excursionDesestresante::Turista->Excursiones->Bool
excursionDesestresante turista = (<=(-3)) . deltaExcursionSegun stress turista

disminuirEn::Int->Int->Int
disminuirEn num = max 0 . flip (-) num


{-a.	Hacer que un turista haga un tour. Esto implica, primero un aumento del 
stress en tantas unidades como cantidad de excursiones tenga el tour, y luego realizar las excursiones en orden.-}

type Tour = [Excursiones]
hacerTour::Tour->Turista->Turista
hacerTour tours unTurista =  foldl(flip hacerExcursión)  (cambiarStress (stress unTurista - length tours)unTurista) tours



{-b.	Dado un conjunto de tours, saber si existe alguno que sea convincente para un turista. Esto significa que el tour 
tiene alguna excursión desestresante la cual, además, deja al turista acompañado luego de realizarla.-}

tourConvenienteParaTurista :: Turista -> [Tour] -> Bool
tourConvenienteParaTurista unTurista  = any(excursionDesestresante unTurista)
