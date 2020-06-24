import Text.Show.Functions()

data Chofer = Chofer{
nombre :: String,
kilometros :: Float,
viaje :: [Viaje],
condicion :: CondiciondeViaje
}deriving (Show)


data Viaje = Viaje{
fecha :: (Int,Int,Int),
cliente :: Cliente,
costo :: Costo
}deriving(Show)

type Costo = Float
data Cliente = Cliente{
nombreCliente :: String,
direccion :: String
}deriving (Show)

type CondiciondeViaje = Viaje-> Bool

cualquierViaje :: CondiciondeViaje
cualquierViaje _ = True

--otros solo toman los viajes que salgan más de $ 200
mayora200 :: CondiciondeViaje
mayora200   = (>200).costo

--otros toman aquellos en los que el nombre del cliente tenga más de n letras

tieneMasDeNletras :: Int -> CondiciondeViaje
tieneMasDeNletras n = (>n).  length .nombreCliente . cliente

--y por último algunos requieren que el cliente no viva en una zona determinada

noViveEn :: String -> CondiciondeViaje
noViveEn locacion = (/=locacion) . direccion . cliente


-- Punto 3
lucas :: Cliente
lucas = Cliente "Lucas" "Victoria"

dani :: Chofer
dani = Chofer "Daniel" 23500 [Viaje (20, 4, 2017) lucas 150] (noViveEn "Olivos")

ale :: Chofer
ale = Chofer "Alejandra" 180000 [] cualquierViaje


puedeTomar:: Viaje -> Chofer -> Bool
puedeTomar unViaje unChofer = condicion unChofer $ unViaje 


sacarCosto :: Chofer -> Float
sacarCosto unChofer = sum (listaDeCostos unChofer)


listaDeCostos :: Chofer -> [Costo]
listaDeCostos unChofer = map costo.viaje $ unChofer

{- dado un viaje y una lista de choferes, se pide que
filtre los choferes que toman ese viaje. Si ningún chofer está interesado, no se preocupen: el viaje no se puede realizar.
-}

sePuedeRezlizar :: Viaje -> [Chofer]-> [Chofer]
sePuedeRezlizar  unViaje losChoferes = filter (puedeTomar unViaje) $ losChoferes

--considerar el chofer que menos viaje tenga. Si hay más de un chofer elegir cualquiera.

cantidadDeViajes :: Chofer -> Int
cantidadDeViajes unChofer= length.viaje $ unChofer

elMenos :: Viaje -> [Chofer] -> Chofer
elMenos unViaje losChoferes = foldl1 menosViajeunChofer.sePuedeRezlizar unViaje $ losChoferes

menosViajeunChofer :: Chofer -> Chofer -> Chofer
menosViajeunChofer  unChofer  otroChofer | cantidadDeViajes unChofer > cantidadDeViajes otroChofer = unChofer
                                         | otherwise =otroChofer


agregarViaje:: Chofer -> Viaje -> Chofer
agregarViaje  unChofer unViaje = unChofer {viaje = unViaje : viaje unChofer}

çnitoInfy :: Chofer
nitoInfy = UnChofer "Nito Infy" 70000 infinitosViajesConLucas (clienteConMasDeNLetras 3)

viajesConLucas :: Viaje
viajesConLucas = UnViaje (11,3,2017) lucas 50

 