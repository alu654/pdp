import Text.Show.Functions()

data Cancion = Cancion{
titulo :: String,
genero :: String,
duracion :: Int
}deriving (Show)


data Artista = Artista{
nombre :: String,
canciones:: [Cancion],
efecto :: Efecto
}deriving (Show)

type Efecto= Cancion -> Cancion

abba:: Cancion
abba = Cancion "Ella y yo en la Playa" "reggue" 150

--[abba, cafeParaDos, fuiHastaAhi,saySo]

cafeParaDos :: Cancion
cafeParaDos = Cancion "Cafe para dos" "rock metalico" 146

fuiHastaAhi :: Cancion 
fuiHastaAhi = Cancion "Fui hasta ahi" "rock" 276

saySo :: Cancion 
saySo = Cancion "saySo" "jazz" 140

losEscarabajos :: Artista
losEscarabajos = Artista "Los Escarabajos" [abba,cafeParaDos,saySo] acortar

adela :: Artista
adela = Artista "Adela" [fuiHastaAhi] remixado

elTigreJoaco :: Artista
elTigreJoaco = Artista "El Tigre Joaco" [] (acustizar 360)

cambiarDuracion  :: Int -> Cancion -> Cancion
cambiarDuracion n unaCancion = unaCancion{duracion = n}

acortar:: Efecto 
acortar  unaCancion 
            | (duracion.cambiarDuracion 60 ) unaCancion > 0  =  cambiarDuracion (duracion unaCancion - 60) unaCancion
            | otherwise = unaCancion


agregarRemix :: String ->  Cancion -> Cancion
agregarRemix  palabra unaCancion=unaCancion{titulo =titulo unaCancion ++ palabra } 

cambioarGenero :: String ->  Efecto
cambioarGenero  palabra unaCancion=unaCancion{genero =palabra } 

remixado ::  Efecto 
remixado  unaCancion =  cambioarGenero "Remixado". agregarRemix "remix". cambiarDuracion (duracion unaCancion * 2) $ unaCancion

tieneGenero:: String -> Cancion -> Bool
tieneGenero unGenero unaCancion = genero unaCancion == unGenero

acustizar :: Int -> Cancion -> Cancion
acustizar duracionC unaCancion  
                | tieneGenero " acustico" unaCancion = unaCancion
                | otherwise = cambioarGenero "acustico" . cambiarDuracion (duracionC) $ unaCancion

metaefecto :: [Efecto]-> Efecto
metaefecto listaEfectos = foldl1 (.) listaEfectos

-- ************ PARTE B ************

vistazo :: Artista -> [Cancion]
vistazo unArtista  = (take 3).esCancionCorta. canciones $ unArtista

esCorta :: Cancion-> Bool
esCorta unaCancion = duracion unaCancion < 150

esCancionCorta ::  [Cancion]-> [Cancion] 
esCancionCorta listaDeCanciones = filter esCorta listaDeCanciones

armarListaCancionesDeArtistas :: (Artista -> [Cancion]) -> [Artista] -> [Cancion]
armarListaCancionesDeArtistas unaFuncion unArtista = concatMap unaFuncion $ unArtista

juntarCanciones :: (Artista -> [Cancion]) -> [Artista] -> [Cancion]
juntarCanciones funcion = concatMap funcion

playlist:: String -> [Artista] -> [Cancion]
playlist unGenero = juntarCanciones (cancionesDe unGenero) 

cancionesDe :: String -> Artista -> [Cancion]
cancionesDe palabra  unArtista = (filter(tieneGenero palabra) . canciones) unArtista

hacerceDj:: Artista -> Artista
hacerceDj unArtista = mapCanciones(map (efecto unArtista))  unArtista

mapCanciones :: ([Cancion] -> [Cancion]) -> Artista -> Artista
mapCanciones funcion unArtista = unArtista {canciones = funcion.canciones $ unArtista}

-- ************ PARTE C *********

tieneGustoHomogeneo :: Artista -> Bool
tieneGustoHomogeneo unArtista = (todasSonDelMismoGenero.canciones) unArtista

sonDelMismoGenero :: Cancion -> Cancion -> Bool
sonDelMismoGenero  unaCancion otraCancion = genero unaCancion == genero otraCancion

todasSonDelMismoGenero :: [Cancion] -> Bool
todasSonDelMismoGenero (cabeza : cola) = all(sonDelMismoGenero cabeza) cola

formarBanda :: String -> [Artista] -> Artista
formarBanda nombreBanda listaArtista = Artista nombreBanda (juntarCanciones canciones listaArtista) (unirEfectos listaArtista)

unirEfectos :: [Artista] -> (Cancion -> Cancion)
unirEfectos unosArtista = metaefecto(map efecto unosArtista)

-- 4

obraMaestraProgresiva :: Artista -> Cancion
obraMaestraProgresiva unArtista = Cancion(concatenarTitulos unArtista)(generoSuperador unArtista) (sumarDuraciones unArtista)

concatenarTitulos :: Artista -> String
concatenarTitulos unArtista = operarCanciones (++) titulo unArtista

operarCon ::( a -> a -> a) -> [a] -> a
operarCon = foldl1

sumarDuraciones :: Artista -> Int
sumarDuraciones = operarCanciones (+) duracion

operarCanciones ::  ( a -> a -> a) -> (Cancion -> a) -> Artista -> a
operarCanciones funcion1 funcion2 = operarCon funcion1 . (obtenerDeCanciones funcion2 ). canciones

obtenerDeCanciones:: (Cancion -> a ) -> [Cancion]-> [a]
obtenerDeCanciones funcion = map funcion



