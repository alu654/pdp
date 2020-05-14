import Text.Show.Functions()

data Participante = UnParticipante { 
nombre               :: String,
cantidadDeDinero     :: Int,
tacticaDeJuego       :: String,
propiedadesCompradas :: [Propiedad],
accionesDelJuego     :: [Accion]
}deriving (Show)

type Accion =  Participante -> Participante
type Propiedad = (String, Int)

carolina :: Participante
carolina = UnParticipante "Carolina" 500 "Accionista" [] [pasarPorElBanco, pagarAAccionistas]

manuel :: Participante
manuel = UnParticipante "Manuel" 500 "Oferente singular" [] [pasarPorElBanco, enojarse] 

-- pasarPorElBanco: aumenta el dinero del jugador en $40 y cambia su táctica a “Comprador compulsivo”.

pasarPorElBanco :: Accion
pasarPorElBanco  unParticipante  =  (cambiarDinero (+40) . cambiartacticaDeJuego "Comprador compulsivo")unParticipante

cambiartacticaDeJuego :: String -> Accion
cambiartacticaDeJuego unaTactica unParticipante = unParticipante {tacticaDeJuego =  unaTactica}

-- enojarse: suma $50 y agrega gritar a sus acciones.

enojarse :: Accion
enojarse unParticipante = (cambiarDinero (+50) . agregarAccion gritar) unParticipante

agregarAccion :: Accion -> Accion
agregarAccion  unAccion unParticipante  = unParticipante {accionesDelJuego = accionesDelJuego unParticipante ++ [unAccion]}


cambiarDinero ::  (Int->Int) -> Participante -> Participante
cambiarDinero unaFuncion unParticipante = unParticipante { cantidadDeDinero = unaFuncion ( cantidadDeDinero unParticipante ) }


-- gritar: agrega “AHHHH” al principio de su nombre.

gritar :: Accion 
gritar unParticipante = unParticipante { nombre  = "AHHHH" ++ nombre unParticipante }


-- subastar: al momento de una subasta solo quienes tengan como tácticas “Oferente singular” o “Accionista” podrán ganar la propiedad. Ganar implica precio el precio de la propiedad de su dinero y sumar la nueva adquisición a sus propiedades. 

subastar :: Propiedad -> Accion
subastar adquirida unParticipante 
    | esAccionistauOferente unParticipante = adquirirPropiedad adquirida unParticipante
    | otherwise = unParticipante

adquirirPropiedad :: Propiedad -> Accion
adquirirPropiedad unaPropiedad unParticipante = (cambiarDinero (subtract (precio unaPropiedad))  .agregarPropiedad unaPropiedad) unParticipante 


agregarPropiedad:: Propiedad -> Participante -> Participante
agregarPropiedad unaPropiedad unParticipante = unParticipante {propiedadesCompradas = unaPropiedad : (propiedadesCompradas unParticipante)   }

precio:: Propiedad-> Int
precio (_, plata)= plata 

esAccionistauOferente :: Participante -> Bool
esAccionistauOferente  unParticipante = esAccionista unParticipante || esOferenteSingular unParticipante 
-- cobrarAlquileres: suma $10 por cada propiedad barata y $20 por cada propiedad cara obtenida. Las propiedades baratas son aquellas cuyo precio es menor a $150.

cobrarAlquileres :: Accion
cobrarAlquileres unParticipante = unParticipante {cantidadDeDinero = cantidadDeDinero unParticipante + (sum.(map costoPropiedad).propiedadesCompradas) unParticipante }

costoPropiedad :: Propiedad -> Int
costoPropiedad unaPropiedad 
    | precio unaPropiedad < 150 = 10
    | otherwise = 20    

--pagarAAccionistas: resta $100 para todos los casos excepto que la táctica sea “Accionista”, en ese caso suma $200.

pagarAAccionistas :: Accion
pagarAAccionistas unParticipante 
    |esAccionista unParticipante = cambiarDinero (+200) unParticipante
    |otherwise =   cambiarDinero (subtract 100) unParticipante

esAccionista :: Participante -> Bool
esAccionista  unParticipante = tacticaDeJuego unParticipante   == "Accionista" 


esOferenteSingular :: Participante -> Bool
esOferenteSingular unParticipante = tacticaDeJuego unParticipante   == "Oferente singular" 

--hacerBerrinchePor: cuando una persona hace un berrinche por una propiedad se le suman $10 y se la hace gritar, la persona sigue haciendo berrinche hasta que llegue a comprar la propiedad que quiere.


hacerBerrinchePor ::  Propiedad -> Accion
hacerBerrinchePor  unaPropiedad unParticipante -- ?
        | puedeComprar unaPropiedad unParticipante = adquirirPropiedad unaPropiedad unParticipante
        | otherwise = hacerBerrinchePor unaPropiedad ((cambiarDinero (+10) . gritar) unParticipante )

--                5           7         True
puedeComprar :: Propiedad -> Participante -> Bool
puedeComprar unaPropiedad unParticipante = cantidadDeDinero unParticipante >= precio unaPropiedad  


--últimaRonda, que dado un participante retorna una acción equivalente a todas sus acciones.
ultimaRonda :: Participante->Accion
ultimaRonda unParticipante = foldl1 (.) (accionesDelJuego unParticipante)

--Hacer una función juegoFinal la cual toma dos participantes y devuelve al ganador.
juegoFinal :: Participante -> Participante -> Participante
juegoFinal unParticipante otroParticipante
    | dineroEnUtimaRonda unParticipante > dineroEnUtimaRonda otroParticipante = jugarUltimaRonda unParticipante
    | otherwise = jugarUltimaRonda otroParticipante


jugarUltimaRonda :: Accion
jugarUltimaRonda unParticipante = (ultimaRonda unParticipante) unParticipante

dineroEnUtimaRonda ::  Participante -> Int
dineroEnUtimaRonda unParticipante = (cantidadDeDinero.jugarUltimaRonda) unParticipante