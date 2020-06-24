import Text.Show.Functions()


-- registrar su nombre, el nivel de reconocimiento y felicidad que tiene, y cuáles son sus deseos

data Humano = Humano {
nombre :: String,
reconocimiento :: Int,
felicidad :: Int,
deseos :: [Deseos]
}deriving (Show)

type Deseos = Humano -> Humano 

camila :: Humano
camila = Humano "Camila" 50 100 [pazMundial, recibirse "Ingeniero en sistemas", recibirse "Medicina" , famoso ]
--- La paz mundial: al cumplirse, el humano se vuelve 20  veces más feliz.
cambiarFelicidad :: Int -> Humano -> Humano
cambiarFelicidad n unHumano =  unHumano{felicidad = n}
 
pazMundial :: Humano -> Humano
pazMundial unHumano = cambiarFelicidad (felicidad unHumano * 20) unHumano

{-- Recibirse de una carrera: al cumplirse, la felicidad del humano aumenta en 250
 y su reconocimiento aumenta en el triple de la longitud del nombre de la carrera de la que se recibió.-}

cambiarreconocimiento :: String -> Humano -> Humano
cambiarreconocimiento n unHumano =  unHumano{reconocimiento = 3 * length n}


recibirse :: String -> Humano -> Humano 
recibirse n unHumano = cambiarFelicidad (felicidad unHumano + 250) . cambiarreconocimiento n $ unHumano

--Ser famoso: al cumplirse, el reconocimiento del humano aumenta en 1000 
--y su felicidad pasa a ser 50 (independientemente de qué tan feliz fuera antes de cumplir el deseo).

famoso :: Humano -> Humano
famoso unHumano = aumentarReconociminento (1000)  . cambiarFelicidad (50) $ unHumano

aumentarReconociminento :: Int -> Humano -> Humano
aumentarReconociminento n unHumano =  unHumano{reconocimiento = reconocimiento unHumano + n}

-- Punto 2
{-2.	Conocer la espiritualidad de un deseo para un humano, que se calcula como la diferencia
 entre lo que gana de felicidad luego de cumplir ese deseo y lo que gana de reconocimiento luego de cumplir ese deseo.-}

espiritualdad :: Humano-> Humano -> Int
espiritualdad unHumano unHumanoOtro = (reconocimiento unHumanoOtro - reconocimiento unHumano) + (felicidad unHumanoOtro - felicidad unHumano)


-- Punto 3
{-Para el humano de ejemplo, luego de cumplir todos sus deseos en el orden indicado en el punto 
1 no sería más feliz, porque al volverse famoso quedaría con felicidad 50.

-}
pruebaFold :: Humano -> Humano
pruebaFold unHumano = foldl1  (flip(.))  (deseos unHumano) $ unHumano


esMasFeliz:: Humano-> Bool
esMasFeliz unHumano = felicidad unHumano > (felicidad.pruebaFold) unHumano 
                
cuatro:: Humano -> Humano -> Bool
cuatro unHumano unHumanoOtro = espiritualdad unHumano unHumanoOtro < 150

data Demonio = Demonio {
    deudores :: [String],
    subordinados :: [Demonio]
} deriving (Show, Eq)

alan :: Demonio 
alan = Demonio ["alan" , "Juan" , ]
formaParte :: Demonio -> Humano -> Bool
formaParte unDemonio unHumano = elem(nombre unHumano) (deudores unDemonio) 

