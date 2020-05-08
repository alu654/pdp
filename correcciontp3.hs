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
pasarPorElBanco unParticipante = unParticipante {cantidadDeDinero  =  cantidadDeDinero  unParticipante + 40 , tacticaDeJuego = "Comprador compulsivo" }

-- enojarse: suma $50 y agrega gritar a sus acciones.

enojarse :: Accion
enojarse unParticipante =  unParticipante {cantidadDeDinero  =  cantidadDeDinero  unParticipante + 50 , accionesDelJuego = accionesDelJuego unParticipante ++ [gritar]} 

-- gritar: agrega “AHHHH” al principio de su nombre.

gritar :: Accion 
gritar unParticipante = unParticipante { nombre  = "AHHHH" ++ nombre unParticipante }


-- subastar: al momento de una subasta solo quienes tengan como tácticas “Oferente singular” o “Accionista” podrán ganar la propiedad. Ganar implica restar el precio de la propiedad de su dinero y sumar la nueva adquisición a sus propiedades. 

subastar :: Propiedad -> Accion
subastar adquirida unParticipante 
    | esAccuOferente unParticipante = agregarPropiedad adquirida unParticipante
    | otherwise = unParticipante

agregarPropiedad :: Propiedad -> Accion
agregarPropiedad ganada unParticipante = unParticipante { cantidadDeDinero = cantidadDeDinero unParticipante - restar ganada, propiedadesCompradas = propiedadesCompradas unParticipante ++ [ganada] }

restar:: Propiedad-> Int
restar (_, plata)= plata 

esAccuOferente :: Participante -> Bool
esAccuOferente  unParticipante = tacticaDeJuego unParticipante == "Oferente singular" || tacticaDeJuego unParticipante == "Accionista"

-- cobrarAlquileres: suma $10 por cada propiedad barata y $20 por cada propiedad cara obtenida. Las propiedades baratas son aquellas cuyo precio es menor a $150.

cobrarAlquileres :: Accion
cobrarAlquileres unParticipante = unParticipante {cantidadDeDinero = cantidadDeDinero unParticipante + (sum.(map unaPropiedadBarata).propiedadesCompradas) unParticipante }

unaPropiedadBarata :: Propiedad -> Int
unaPropiedadBarata unaPropiedad 
    | snd unaPropiedad < 150 = 10
    | otherwise = 20    

--pagarAAccionistas: resta $100 para todos los casos excepto que la táctica sea “Accionista”, en ese caso suma $200.

pagarAAccionistas :: Accion
pagarAAccionistas unParticipante 
    |restaAccionistas unParticipante = unParticipante {cantidadDeDinero = cantidadDeDinero unParticipante + 200}
    |otherwise = unParticipante {cantidadDeDinero = cantidadDeDinero unParticipante - 100}

restaAccionistas :: Participante -> Bool
restaAccionistas  unParticipante = tacticaDeJuego unParticipante == "Accionista" 
