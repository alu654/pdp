module Taller where
import Text.Show.Functions ()

type Genero = Festival -> Festival
type Animo = String

data Banda = Banda {
  nombre :: String,
  genero :: Genero,
  descripciones :: [String],
  decibeles :: Float
} deriving (Show)

data Festival = Festival {
  lugar :: String,
  cantPub :: Float,
  animPub :: Animo,
  cronograma :: [Banda]
} deriving (Show)

festichola = Festival {
  lugar = "La Casa de tu Vieja",
  cantPub = 1000,
  animPub = "Indiferente",
  cronograma = [theStrokers, metalica, soda, losRedondos]
} 

tocar :: Banda -> Festival -> Festival
tocar banda festi = genero banda festi

cantPubModificar f festi = festi {cantPub = f (cantPub festi)}
animPubModificar f festi = festi {animPub = f (animPub festi)}

rockNacional :: Genero
rockNacional = cantPubModificar (100+)

pop :: Genero
pop festi
  | animPub festi == "Indiferente" = animPubModificar (\_ -> "Euforico") (cantPubModificar (2*) festi)
  | otherwise = festi

metal :: String -> Genero
metal "heavy" festi = haceMetal (\p -> p  ++ " pesado") festi
metal "trash" festi = haceMetal (\p -> " basura" ++ p) festi

haceMetal f festi = animPubModificar f (cantPubModificar (*1.01) festi)

losRedondos = Banda {
  nombre = "Los Redondos",
  genero = rockNacional,
  descripciones = ["legendaria","pogosa"],
  decibeles = 45
} 

soda = Banda {
  nombre = "Soda",
  genero = rockNacional,
  descripciones = ["irrepetible"],
  decibeles = 40
}

miranda = Banda {
  nombre = "Miranda",
  genero = pop,
  descripciones = ["insípida", "incolora", "inodora"],
  decibeles = 60
}

metalica = Banda {
  nombre = "Metalica",
  genero = metal "heavy",
  descripciones = ["legendaria", "vendida"],
  decibeles = 60
}

basura = Banda {
  nombre = "Eto no e tra",
  genero = metal "trash",
  descripciones = ["lo tuio e", "basura"],
  decibeles = 45000
}

theStrokers = Banda {
  nombre = "The Strokers",
  genero = pop.metal "heavy",
  descripciones = ["suicidio asistido", "emocional","linda"],
  decibeles = 45
}

tocar' festi banda = tocar banda festi

suceder :: Festival -> Festival
suceder festi = foldl tocar' festi (cronograma festi)

{-
esAlgo :: Banda -> [String]
esAlgo banda
  | vendida banda && acustica banda && legendaria banda = ["Vendida","Acustica","Legendaria"]
  | vendida banda && acustica banda = ["Vendida","Acustica"]
  | vendida banda && legendaria banda = ["Vendida","Legendaria"]
  | acustica banda && legendaria banda = ["Acustica","Legendaria"]
  | vendida banda = ["Vendida"]
  | acustica banda = ["Acustica"]
  | legendaria banda = ["Legendaria"]
  | otherwise = []
-}
type Clasificacion = Banda -> Bool

vendida :: Clasificacion
vendida banda = 3 < length (descripciones banda) || elem "vendida" (descripciones banda)

--Podria usar aplicacion parcial sacando la palabra banda de ambos lados, pero para mi es mas facil de entender con la palabra banda
acustica :: Clasificacion
acustica banda = 45 < decibeles banda

legendaria :: Clasificacion
legendaria banda = elem "legendaria" (descripciones banda) && 40 < decibeles banda

popularidad :: [Clasificacion] -> Banda -> Int
popularidad listaClasi banda = 100 * length (filter (seraAlgo banda) listaClasi)

seraAlgo banda clasi = clasi banda

--buenFest :: Festival -> [Clasificacion] -> Bool
--buenFest festi listaClasi = buenCronologico festi listaClasi && buenPopularidad festi listaClasi

buenCronologico :: Festival -> [Clasificacion] -> Bool
buenCronologico festi listaClasi = recursivaCRONO (cronograma festi) listaClasi 0

recursivaCRONO :: [Banda] -> [Clasificacion] -> Int -> Bool
recursivaCRONO [] _ popu = True
recursivaCRONO (x:xs) listaClasi popu 
  | popu < popularidad listaClasi x = recursivaCRONO xs listaClasi (popularidad listaClasi x)
  | otherwise = False

buenPopularidad :: Festival -> [Clasificacion] -> Bool
buenPopularidad festi listaClasi = 1000 < ( sum (listaPopularidades listaClasi (cronograma festi)))

listaPopularidades :: [Clasificacion] -> [Banda] -> [Int]
listaPopularidades listaClasi crono = map (popularidad listaClasi) crono 










































