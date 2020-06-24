import Text.Show.Functions

data Festival= Festival{
lugar :: String,
cantPub :: Int,
estadAni :: String,
bandas :: [Banda]
}deriving (Show)

data Banda = Banda {
  nombre :: String,
  genero :: Genero,
  descripciones :: [String],
  decibeles :: Float
} deriving (Show)

type Genero = Festival -> Festival

--De cada festival se sabe el lugar donde se realiza, la cantidad y estado de ánimo inicial del público y las bandas que tocan en él, ordenadas cronológicamente.
--Por ejemplo, el festival Hullabalooza, en el que tocan Miranda, Los Redondos, Metallica y Soda, tiene un público de 20000 personas con ánimo inicial “indiferentte”.

{-Los redondos, que está descripta como “legendaria” y “pogosa”, toca a 45 decibeles y se considera de rock nacional.
Soda está descripta como "irrepetible", toca a 40 decibeles y también es de rock nacional.
Miranda es una banda de pop que toca a 60 decibeles y los críticos la describieron como "insípida", "incolora" e "inodora".
Metallica está descripta como “legendaria” y “vendida” y toca a 60 decibeles. Es una de las mayores exponentes del heavy metal.
Como se observa con el rock nacional, puede haber muchas bandas de cualquiera de los géneros.-}


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
  genero = metalheavy,
  descripciones = ["legendaria", "vendida"],
  decibeles = 60
}

basura = Banda {
  nombre = "Eto no e tra",
  genero = trashmetal "trash",
  descripciones = ["lo tuio e", "basura"],
  decibeles = 45000
}

theStrokers = Banda {
  nombre = "The Strokers",
  genero = popmetal "heavy",
  descripciones = ["suicidio asistido", "emocional","linda"],
  decibeles = 45
}

{-rock nacional: hace que el público aumente en 100 personas
pop: generalmente no afectan al público, sólo en caso que el estado de ánimo sea "indiferente", duplican la cantidad y dejan el público "eufórico".
Otro género que suele estar presente en los festivales es el metal, que tiene variantes que los especialistas denominan subgéneros
Heavy metal: hace que el público aumente 1% cada vez que toca, y a su estado de ánimo se le agregue “pesado” al final.
Trash metal: Hace que el público aumente 1% al tocar y se le agrega "basura" al final del estado de ánimo.
Existen otros subgéneros del metal que también alteran al público de igual manera, pero agregando otros calificativos al estado de ánimo.
-}

cambiarPublico :: Int-> Festival-> Festival
cambiarPublico n unaFestival= unaFestival { cantPub = n}

cambiarAnimPublico :: String-> Festival-> Festival
cambiarAnimPublico n unaFestival= unaFestival { estadAni =n }

rockNacional :: Festival -> Festival
rockNacional unaFestival = cambiarPublico (cantPub unaFestival + 10) unaFestival


pop :: Festival -> Festival
pop unaFestival 
            | estadAnimo "Indiferente" unaFestival = cambiarPublico (cantPub unaFestival * 2 ) . cambiarAnimPublico ("euforico") $ unaFestival
            | otherwise = unaFestival


estadAnimo ::String ->  Festival -> Bool
estadAnimo  aloja unaFestival = estadAni unaFestival == aloja

{-Otro género que suele estar presente en los festivales es el metal, que tiene variantes que los especialistas denominan subgéneros
Heavy metal: hace que el público aumente 1% cada vez que toca, y a su estado de ánimo se le agregue “pesado” al final.
Trash metal: Hace que el público aumente 1% al tocar y se le agrega "basura" al final del estado de ánimo.-}



metalheavy :: Festival -> Festival
metalheavy unFestival = cambiarPublico ( cantPub unFestival * 10 ) . cambiarEstAnim ("pesad") $ unFestival

cambiarEstAnim :: String -> Festival -> Festival
cambiarEstAnim n unFestival = unFestival {estadAni = n }

trashmetal :: String -> Genero
trashmetal n unFestival = cambiarPublico (cantPub unFestival + (cantPub unFestival * 10)) . cambiarEstAnim (estadAni unFestival ++ n ) $ unFestival

popmetal :: String -> Genero
popmetal n unFestival = cambiarEstAnim (n) unFestival


-- Definir la función suceder, que hace que suceda un festival. El resultado debe ser el mismo festival pero 
--con el público en su situación final, luego de haber tocado todas las bandas.

--suceder :: Genero 
--suceder unFestival = foldl1 (+) cantPub $unFestival

{-Se conocen ciertos criterios de clasificación de bandas, de los cuales depende su popularidad. Por ejemplo:
Vendida: Debe tener tres o más descripciones o bien una descripción que sea “vendida”.
Acústica: Es la que toca a más de 55 decibeles.
Legendaria. Debe estar descripta como “legendaria” y tocar a más de 40 decibeles.
Definir las funciones que permitan clasificar a las bandas. Una banda puede clasificarse de más de una manera a la vez o ninguna.-}


type Clasificaciones = Banda -> Bool
vendida :: Clasificaciones
vendida  = (>3). (length.descripciones)

acústica :: Clasificaciones
acústica unaBanda = tocaAMas 55 unaBanda 

legendaria :: Clasificaciones
legendaria banda = elem "legendaria" (descripciones banda) && 40 < decibeles banda

tocaAMas :: Float -> Clasificaciones
tocaAMas n banda = (>n) (decibeles banda) 


cambiarDecibeles :: Float -> Banda-> Banda
cambiarDecibeles n unaBanda = unaBanda {decibeles = n }

cambiarDes :: String -> Banda-> Banda
cambiarDes n unaBanda = unaBanda {descripciones = [n] }


cantidadDeClasificaciones :: [Clasificaciones] -> Banda -> Int
cantidadDeClasificaciones listaClasi banda =100 * length (filter (seraAlgo banda) listaClasi)

seraAlgo banda clasi = clasi banda