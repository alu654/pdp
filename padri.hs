import Text.Show.Functions()

--timmy = Chico “Timmy” 10 [“mirar television”, “jugar en la pc”] [serMayor]
type Habilidad = String
type Deseo = Chico -> Chico
type Condicion =  Chico -> Bool

data Chico = Chico{
nombre:: String,
edad :: Int,
habilidades :: [Habilidad],
deseos :: [Deseo]
}deriving (Show)

data Chica = CrearChica {
    nombreChica :: String,
    condicion :: Condicion
}



timmy :: Chico
timmy = Chico "Timmy" 10 ["cantar","cocinar","enamorar"] [serMayor]

jorge :: Chico
jorge = Chico "jorge" 14 ["ser un supermodelo noruego"] [ serMayor, serGrosoEnNeedForSpeed]

cristina :: Chica
cristina = CrearChica "cristina" sabeCocinar 

 


-- aprenderHabilidades habilidades unChico : agrega una lista de habilidades
--nuevas a las que ya tiene el chico.

aprenderHabilidades :: [Habilidad] -> Chico -> Chico
aprenderHabilidades listaDeHabilidades unChico = unChico{habilidades = listaDeHabilidades ++ habilidades unChico}

{-serGrosoEnNeedForSpeed unChico: dado un chico, le agrega las habilidades
de jugar a todas las versiones pasadas y futuras del Need For Speed, que
son: “jugar need for speed 1”, “jugar need for speed 2”, etc-}




serGrosoEnNeedForSpeed:: Deseo
serGrosoEnNeedForSpeed unChico =  unChico{habilidades = habilidades unChico ++ agregarNeed}

agregarNeed :: [Habilidad]
agregarNeed  =  map agregarNumero [1..] 

agregarNumero :: Int -> String
agregarNumero unNumero = "jugar need for speed " ++ (show unNumero)


--serMayor unChico: Hace que el chico tenga 18 años.
serMayor :: Chico -> Chico
serMayor unChico = cambiarEdad 18 unChico

{-wanda: dado un chico, wanda le cumple el primer deseo y lo hace madurar
(crecer un año de edad).-}

wanda :: Deseo
wanda unChico = primerDeseo.madurar $ unChico

primerDeseo :: Deseo
primerDeseo unChico = ((head.deseos) unChico) unChico

madurar:: Chico-> Chico
madurar unChico = cambiarEdad (edad unChico + 1) unChico

cambiarEdad ::Int-> Chico -> Chico
cambiarEdad  valor unChico =unChico{edad = valor} 

{-cosmo: dado un chico, lo hace “des”madurar, quedando con la mitad de años de
edad. Como es olvidadizo, no le concede ningún deseo-}

desmadurar:: Chico -> Chico
desmadurar unChico = cambiarEdad (div(edad unChico) 2) unChico

cosmo :: Chico -> Chico
cosmo = desmadurar


{-muffinMagico: dado un chico le concede todos sus deseos.
Nota importante: no debe haber lógica repetida entre wanda, cosmo y serMayor-}

concederTodosLosDeseos :: Chico -> Chico
concederTodosLosDeseos unChico = foldl1 (.) (deseos unChico) $ unChico

muffinMagico :: Chico -> Chico
muffinMagico = concederTodosLosDeseos

{-. tieneHabilidad habilida unChico: Dado un chico y una habilidad, dice
si la posee.-}

tieneHabilidad :: Habilidad -> Condicion 
tieneHabilidad   habilidad unChico = elem habilidad (habilidades unChico)


{-esSuperMaduro: Dado un chico dice si es mayor de edad (es decir, tiene más
de 18 años) y además sabe manejar.-}

superMaduro:: Chico -> Bool
superMaduro  = (>18).edad

esSuperMaduro:: Condicion
esSuperMaduro unChico = superMaduro unChico && tieneHabilidad "Manejar" unChico

sabeCocinar :: Chico -> Bool
sabeCocinar unChico = tieneHabilidad "Cocinar" unChico

{-quienConquistaA unaChica losPretendientes: Dada una chica y una lista
de pretendientes, devuelve al que se queda con la chica, es decir, el primero
que cumpla con la condición que ella quiere. Si no hay ninguno que la cumpla,
devuelve el último pretendiente (una chica nunca se queda sola). (Sólo en este
punto se puede usar recursividad)-}

esHabilidadProhibida :: Habilidad -> Bool
esHabilidadProhibida "matar" = True 
esHabilidadProhibida "enamorar" = True
esHabilidadProhibida "dominar el mundo" = True
esHabilidadProhibida _ = False

tomarloscinco::Chico -> [Habilidad]
tomarloscinco unChico = take 5 (habilidades unChico)


contieneHabilidadesPrhibidas:: Condicion
contieneHabilidadesPrhibidas unChico = any esHabilidadProhibida (tomarloscinco unChico)


noFormanParte:: [Chico] -> [Chico]
noFormanParte = filter(contieneHabilidadesPrhibidas)


