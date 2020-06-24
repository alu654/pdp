import Text.Show.Functions()
-- nombre, una cantidad de poder y una lista de derrotas (léase, las veces que derrotó a alguien).
-- Cada derrota tiene el nombre de su oponente y el año en el que ocurrió.

data Personaje = Personaje {
nombre :: String,
cantidadDePoder :: Int,
derrotas :: [Derrota],
equipamiento :: [Equipamiento]
}deriving (Show)

type Derrota = (String, Int)
type Equipamiento = Personaje -> Personaje
type P = Personaje -> Personaje

capitanAmerica :: Personaje
capitanAmerica = Personaje "Capitan America" 501 [("Hijo de Thanos",1997)] []

thanos :: Personaje
thanos = Personaje "Thanos" 4100 [] [{-gemaDelAlma-}]


{-Modelar entrenamiento, el cual lo realizan un grupo de personajes y
multiplica el poder de cada uno de ellos por la cantidad de personajes que están entrenando al mismo tiempo-}

multiplicarPoder :: Int -> P
multiplicarPoder  entrenando unPersonaje = unPersonaje{cantidadDePoder = cantidadDePoder unPersonaje * entrenando}


entrenamiento :: [Personaje] -> [Personaje]
entrenamiento lista = map (multiplicarPoder (length lista))  lista

--Son dignos aquellos personajes que, luego de haber entrenado,  tienen un poder mayor a 500 y además alguna de sus derrotas se llame "Hijo de Thanos".

suPoderEsMayorA500 ::Int ->  Personaje -> Bool
suPoderEsMayorA500 n unPersonaje = cantidadDePoder unPersonaje  > n

fueDerrotadoPorHijoDeThanos ::String->  Personaje -> Bool
fueDerrotadoPorHijoDeThanos forro unPersonaje = nombre unPersonaje == forro

rivalesDignos :: [Personaje] -> [Personaje]
rivalesDignos unosPersonajes = filter esRivalDigno.entrenamiento $ unosPersonajes

esRivalDigno :: Personaje -> Bool
esRivalDigno unPersonaje = suPoderEsMayorA500 500 unPersonaje && fueDerrotadoPorHijoDeThanos  "Hijo De Thanos" unPersonaje

pelear :: Int -> (Personaje,Personaje) -> Personaje
pelear anioDeLaGuerra (unPersonaje,otroPersonaje) 
    | cantidadDePoder unPersonaje > cantidadDePoder otroPersonaje = agregarDerrota unPersonaje (nombre otroPersonaje) anioDeLaGuerra
    | otherwise = agregarDerrota otroPersonaje (nombre unPersonaje) anioDeLaGuerra

agregarDerrota :: Personaje  -> String -> Int -> Personaje
agregarDerrota unPersonaje  nombreDeLaDerrota anioDeLaGuerra = unPersonaje {derrotas = (nombreDeLaDerrota, anioDeLaGuerra) : derrotas unPersonaje}

--escudo: si tiene menos de 5 derrotas le suma 50 de poder, pero si tiene 5 o más le resta 100 de poder. 

seLlama:: String -> Personaje -> Bool
seLlama unNombre unPersonaje = (==unNombre).nombre $ unPersonaje

escudo :: Equipamiento
escudo unPersonaje 
    | (cantDerrotas unPersonaje) < 5 = sumarPoder 50 unPersonaje
    | otherwise = sumarPoder (-100) unPersonaje

sumarPoder :: Int -> Equipamiento
sumarPoder n unPersonaje = unPersonaje{cantidadDePoder = n}

cantDerrotas :: Personaje ->Int
cantDerrotas unPersonaje  =length.derrotas $ unPersonaje 


agregarNombreAdelante :: String -> Personaje -> Personaje
agregarNombreAdelante unNombre unPersonaje = unPersonaje {nombre = unNombre ++ " " ++ nombre unPersonaje}

agregarVersionAlFinal :: Int -> Personaje -> Personaje
agregarVersionAlFinal unaVersion unPersonaje = unPersonaje {nombre = nombre unPersonaje ++ " V" ++ show unaVersion}

trajeMecanizado :: Int -> Equipamiento
trajeMecanizado unaVersion unPersonaje = agregarVersionAlFinal  unaVersion . agregarNombreAdelante "Iron"$ unPersonaje

--stormBreaker: Le agrega "dios del trueno" al final del nombre y limpia su historial de derrotas ya que un dios es bondadoso.
finalNombre:: String-> Personaje -> Personaje
finalNombre  n unPersonaje =  unPersonaje {nombre =  nombre unPersonaje ++ n}

stormBreaker :: Equipamiento
stormBreaker unPersonaje 
    | seLlama "Thor" unPersonaje = agregarNombreAdelante "Dios del trueno".limpiarHis $ unPersonaje
    | otherwise = unPersonaje


limpiarHis :: P
limpiarHis unPersonaje = unPersonaje {derrotas = []}

agregarExtrasADerrotas :: Equipamiento
agregarExtrasADerrotas unPersonaje = unPersonaje {derrotas = derrotas unPersonaje ++ crearListaDeExtras }

crearListaDeExtras :: [Derrota]
crearListaDeExtras = map agregarString (zip [1..] [2020..])

agregarString :: (Int,Int) -> Derrota
agregarString (unNumero,unAnio)= ("extra numero " ++ show unNumero, unAnio)