import Text.Show.Functions

data Ladron = Ladron{
nombre :: String,
habilidades :: [Habiliad],
armas :: [Arma]
}deriving (Show)

type Arma= (Rehen -> Rehen)

data Rehen = Rehen{
niveldeComplot :: Int,
nombreRehen :: String,
miedo :: Int,
plan :: [Planes]
}deriving (Show)

type Habiliad = String
type Planes = Ladron -> Ladron
type Intimidacion= Ladron -> Rehen -> Rehen



{-tokio, sabe hacer el “trabajo psicológico”, y “entrar en moto”. Lleva dos pistolas calibre 9
milímetros y una ametralladora de 30 balas.
b. profesor, sabe “disfrazarse de linyera”, “disfrazarse de payaso” y “estar siempre un paso
adelante”. No tiene armas
c. pablo, el cual tiene 40 de complot y 30 de miedo. Su plan es esconderse.
d. arturito, tiene 70 de complot y 50 de miedo. Su plan es esconderse y luego atacar con
pablo.-}

tokio :: Ladron
tokio = Ladron  "Tokio" ["trabajo psicológico", "entrar en moto"] [pistola 9 ,pistola 9, ametralladora 30 ]

profesor :: Ladron
profesor = Ladron  "Profesor" ["disfrazarse de linyera", "disfrazarse de payaso" , "estar siempre un paso adelante"]  []

pablo :: Ladron -> Rehen
pablo ladron = Rehen 40 "Pablo" 30 [esconderse (habilidades ladron)]

arturito ::   Ladron -> Rehen
arturito  ladron = Rehen 70  "Arturito" 50 [esconderse (habilidades ladron), atacarLadron "Pablo" ]

--su nivel de complot, su nivel de miedo, y su plan contra los ladrones,
-- la cual puede involucrar a algún otro rehén.

--Pistola: reduce el nivel de complot de un rehén en 5 veces su calibre, y aumenta su miedo en 3 por
--la cantidad de letras de su nombre

cambiarniveldeComplot :: Int -> Rehen -> Rehen
cambiarniveldeComplot  n rehen= rehen { niveldeComplot = n}

cambiarMiedo:: Int -> Rehen -> Rehen
cambiarMiedo  n rehen= rehen { miedo = n}


pistola :: Int-> Rehen -> Rehen
pistola calibre rehen= cambiarMiedo(miedo rehen + 3 * length( nombreRehen rehen)). cambiarniveldeComplot ( niveldeComplot rehen + calibre + 5) $ rehen

--Ametralladora: siempre reduce el nivel de complot a la mitad, y aumenta su miedo en la cantidad
--de balas que le quedan.

ametralladora :: Int ->  Rehen -> Rehen
ametralladora balas rehen = cambiarniveldeComplot(div (niveldeComplot rehen ) 2 ) . cambiarMiedo (balas ) $rehen

--Disparos: disparar al techo como medida disuasiva. Se usa el arma que le genera más miedo al rehén intimidado.

disparos ::Ladron-> Rehen -> Rehen
disparos  ladron rehen  = (foldl1 (armaIntimidacion rehen) . armas $ ladron) rehen

armaIntimidacion :: Rehen -> Arma -> Arma -> Arma
armaIntimidacion rehen unArma otroArma | (miedo . unArma) rehen > (miedo. otroArma) rehen = unArma
                                       |otherwise = otroArma
 
-- acerse el malo:
-- ○ Cuando el que se hace el malo es Berlín, aumenta el miedo del rehén tanto como la cantidad de
-- letras que sumen sus habilidades.

esmalo :: Ladron -> Rehen -> Rehen
esmalo unLadron rehen | esNombre "Berlin" unLadron = cambiarMiedo (tamañoDeLetras unLadron) rehen 
                      | esNombre "Rio" unLadron = cambiarniveldeComplot (20) rehen
                      | otherwise = cambiarMiedo (miedo rehen + 10 ) rehen


esNombre ::String ->  Ladron -> Bool
esNombre nombreaa ladron = nombre ladron == nombreaa

concathabilidades :: Ladron -> String
concathabilidades  = concat .habilidades 

tamañoDeLetras :: Ladron -> Int
tamañoDeLetras = length .concathabilidades

--Atacar al ladrón: le quita tantas armas como la cantidad de letras del nombre de su compañero,dividido por 10.

atacarLadron :: String -> Ladron -> Ladron
atacarLadron nombreCompañero unLadron = unLadron{armas = take (cantidadDeLetras(nombreCompañero)).armas $ unLadron}

cantidadDeLetras :: String -> Int
cantidadDeLetras nombreCompañero = div(length nombreCompañero)10


--Esconderse: Hace que un ladrón pierda una cantidad de armas igual a su cantidad de habilidades dividido 3.

cantidadDeHabiliades :: Int -> [Habiliad] -> Int
cantidadDeHabiliades n habili = div(length habili)n

esconderse :: [Habiliad] -> Ladron -> Ladron
esconderse habili unLadron = unLadron{armas = take (cantidadDeHabiliades 3 (habili)).armas $ unLadron}


--Saber si un ladrón es inteligente. Ocurre cuando tiene más de dos habilidades. El Profesor es la mente maestra, por lo que indudablemente es inteligente.

esInteligente ::  Ladron -> Bool
esInteligente  unLadron = cantidadDeHabiliades 1 (habilidades  unLadron) > 2

--Que un ladrón consiga un arma nueva, y se la agregue a las que ya tiene.
agregarArma ::Arma -> Ladron -> Ladron
agregarArma armaAAgregar unLadron = unLadron{armas = armaAAgregar : armas unLadron }

-- Que un ladrón intimide a un rehén, usando alguno de los métodos planeados.

intimideA :: Intimidacion -> Ladron -> Rehen -> Rehen
intimideA  elPlan unLadron unRehen  = elPlan unLadron unRehen

-- Que un ladrón calme las aguas, disparando al techo frente a un grupo de rehenes, de los cuales se calman los que tengan más de 60 de complot.

--Saber si un ladrón puede escaparse de la policía. Esto se cumple cuando alguna de las habilidades del ladrón empieza con “disfrazarse de”.

seEscapa ::  Ladron -> Bool
seEscapa  unLadron = funcionAny (map (take 14) . habilidades $ unLadron)

funcionAny ::[String] -> Bool
funcionAny lista = any (=="disfrzarse de") lista


{-Saber si la cosa pinta mal, que es cuando dados unos ladrones y unos rehenes, el nivel de complot
promedio de los rehenes es mayor al nivel de miedo promedio multiplicado por la cantidad de
armas de los ladrones.-}

pintaMal :: [Ladron] -> [Rehen] -> Bool
pintaMal ladrones rehenes =  promedioRehen rehenes > nombrQeQuieras rehenes ladrones

promedioRehen :: [Rehen] -> Int
promedioRehen rehen = div(sum.map niveldeComplot $ rehen ) . length $ rehen

nombrQeQuieras :: [Rehen] -> [Ladron] -> Int
nombrQeQuieras rehen ladrones = promedioMiedo rehen + cantidadArmas ladrones

promedioMiedo :: [Rehen]  -> Int
promedioMiedo rehen =div(sum.map miedo $ rehen ) . length $ rehen

cantidadArmas :: [Ladron] -> Int
cantidadArmas ladrones = sum.map (length.armas)  $ ladrones

{-Que los rehenes se rebelen contra un ladrón, usando el plan que tengan en mente. Saben que es
mala idea, por lo que todos pierden 10 de complot antes de comenzar la rebelión.-}

rebelen :: [Rehen] -> [Rehen]
rebelen rehenes = map (modificarNivelComplot (10))  $ rehenes

modificarNivelComplot :: Int->Rehen->Rehen
modificarNivelComplot valor rehen = rehen{niveldeComplot= niveldeComplot rehen - valor}

