import Text.Show.Functions ()

--De cada persona sabemos su nombre, su dirección, su dinero disponible, su comida favorita y los cupones con los que cuenta.
--De las comidas sabemos su nombre, su costo y los ingredientes que posee.

data Persona = Persona{
nombre :: String,
direccion :: String,
dinero :: Int,
comidaFavorita :: [Comida],
cupones :: [Cupones]
}deriving (Show)

data Comida = Comida{
nombreComida :: String,
costo :: Int,
ingredientes :: [String]
}deriving (Show)

type Cupones = Comida ->Comida
{-Crear a Paula, que vive en Thames al 1585, tiene como comida favorita la hamburguesa deluxe, $3600 en su cuenta y, por ser una usuaria nueva, 
aún no tiene cupones.
Crear la hamburguesa deluxe, la cual cuesta $350 y sus ingredientes son: pan, carne, lechuga, tomate, panceta, queso y huevo frito.
Create a vos (teniendo como mínimo 2 cupones) y a tu comida favorita (si es la hamburguesa deluxe, pone tu segunda favorita). No pongas tu dirección real.-}

paula :: Persona
paula = Persona "Paula" "Thames al 1585" 3600 [hamburgesaDelux]  []

hamburgesaDelux :: Comida
hamburgesaDelux = Comida "Hamburguesa Delux" 350 ["Pan", "Carne","Lechuga", "Tomate" , "Panceta","Queso" , "huevo frito "]

alan :: Persona
alan = Persona "Alan" "Laprida y Savedra" 500 [ribsAlaBarbacoa] []

ribsAlaBarbacoa :: Comida
ribsAlaBarbacoa = Comida "Ribs A la Barbacoa" 450 ["Costillas de cerdo", "Barbacoa", "Papas Fritas"]




{-Siendo el principal objetivo de la aplicación la compra de comidas vamos a implementar dos funcionalidades centrales:

comprar: cuando una persona compra una comida se descuenta el costo de la misma de su dinero disponible (¡ojo! No se puede comprar si no alcanza la plata).
 Además, si salió menos de $200 se vuelve su nueva comida favorita.

carritoDeCompras: en nuestra aplicación se pueden comprar muchas comidas al mismo tiempo. Lamentablemente usar este servicio hace que el empaque sea más 
pesado, por lo que se suma $100 más al total.-}



comprar :: Comida->Persona->Persona
comprar comida unaPersona | mayor comida unaPersona = agregarComida comida.cambiarDinero (costo comida) $ unaPersona
                          | otherwise = unaPersona

comprar2 persona comida = comprar comida persona 

mayor :: Comida->Persona->Bool
mayor comida persona = dinero persona > costo comida 

cambiarDinero :: Int->Persona->Persona
cambiarDinero plata persona = persona {dinero = dinero persona - plata}

comidaFav :: Comida->Persona->Persona
comidaFav comida persona | costo comida < 200 = agregarComida comida persona
                                            | otherwise = persona

agregarComida :: Comida->Persona->Persona
agregarComida comida persona = persona {comidaFavorita =  comida : (comidaFavorita persona)  }


restarDinero :: Int -> Persona -> Persona
restarDinero monto persona = persona {dinero = (dinero persona) - monto}

carritoDeCompras :: [Comida] -> Persona -> Persona
carritoDeCompras listaProductos persona =  restarDinero 100 (foldl comprar2 persona listaProductos)

--semanaVegana: si la comida a comprar es vegana (no contiene carne, huevos o queso entre sus ingredientes) su costo se reduce a la mitad.

semanaVegana :: Comida->Comida
semanaVegana comida | esVegana comida= descuentosDecomida (div (costo comida) 2) comida
                    | otherwise = comida

esVegana ::Comida ->Bool
esVegana comida = any esComidaVegana (ingredientes comida)

esComidaVegana ::String ->Bool
esComidaVegana comida = "carne"==comida || "huevos"==comida || "queso"==comida

descuentosDecomida:: Int ->Comida->Comida
descuentosDecomida descuento comida = comida {costo = costo comida - descuento}


{-esoNoEsCocaPapi: algunas personas gustan de acompañar sus comidas con bebidas espirituosas. Dada una bebida, se agregan "Party" 
al final del nombre de la comida y esta bebida a la lista de ingredientes. El precio, increíblemente, no se ve afectado.-}
type Bebida = String

esoNoesCoca :: Bebida -> Comida -> Comida
esoNoesCoca bebida comida = comida {nombreComida = nombreComida comida ++ "Party", ingredientes = ingredientes comida ++ [bebida]}


--sinTACCis: a todos los ingredientes les agrega "libre de gluten" al final.
sinTACC  :: Comida -> Comida
sinTACC comida = comida {ingredientes = map (\ ing -> ing ++ " libre de gluten") (ingredientes comida)}

--findeVegetariano: en caso que la comida a comprar no contenga carne, el costo se reduce en un 30%.

findeVegetariano :: Comida -> Persona -> Persona
findeVegetariano unaComida unaPersona = comprar (vegetariano unaComida unaPersona) unaPersona

vegetariano :: Comida -> Persona -> Comida
vegetariano unaComida unaPersona
    | noTieneIngredientes "carne" unaComida =cambiarCosto (+ (sacarPorcentaje 30)) unaComida
    | otherwise = unaComida

sacarPorcentaje :: Int -> Int
sacarPorcentaje unValor = div unValor 100

cambiarCosto :: Int -> Comida -> Comida
cambiarCosto n comida = comida {costo = costo comida * n}

{-largaDistancia: este cupón es muy útil para las personas que viven lejos. Por solo $50 pesos mas, Pdeppi puede llevar la comida hasta tu casa. 
¡Al parecer la cantidad de letras de un ingrediente afecta su peso! Así que, lamentablemente, todos los ingredientes que tienen más de 10 
letras se pierden en el camino.-}

type Ingredientes = String
largaDistancia :: Comida -> Comida 
largaDistancia comida = aumentarCosto 50 ((ingredientesFiltrados ingredientes) comida)

aumentarCosto:: Int -> Comida -> Comida
aumentarCosto monto comida= comida {costo = (costo comida) + monto}

ingredientesFiltrados :: [Ingredientes] -> [Ingredientes]
ingredientesFiltrados listaIngredientes = filter ((\ ing -> 10>length ing)) listaIngredientes


--comprarConCupones: nos permite que una persona realice la compra de su comida favorita aplicándole todos los cupones que tiene a su disposición.
comprarConCupones :: Persona -> Persona
comprarConCupones unaPersona = comprar ((foldl1 (.) (cupones unaPersona)) (head (comidaFavorita unaPersona))) unaPersona

--superComida: dado un conjunto de comidas. Se genera una gran comida, que su precio es la sumatoria de todos los precios,
-- su nombre es el conjunto de todos los nombres sacando las vocales y sus ingredientes son todos los ingredientes juntos sin repetidos.


fSumaCosto :: [Comida] -> Comida
fSumaCosto listaComidas = Sum (map costo listaComidas)

comidasSuper listaComidas = comida{
ingredientes = fsumaIngredientes listaComidas,
costo = fsumacosto listacomidas,
nombreComida = fSumaComida listaComidas
} 

fSumaNombreComida listaComidas = concat (map nombreComida listaComidas)

esVocal :: Char->Bool
esVocal 'a'=False
esVocal 'e'=False
esVocal 'i'=False
esVocal 'o'=False
esVocal 'u'=False
esVocal _= True

nombreSuperComida :: [Comida]->String
nombreSuperComida listaComida = filter esVocal.concat.map nombreComida $ listaComida

fsumacosto :: [Comida] -> Int
fsumacosto listaComidas = sum (map costo listaComidas)

--compraDeluxe: hace que una persona compre una súper comida creada a partir de un conjunto de comidas. 
--Para crearla solo se utilizarán aquellas que cuesten menos de $400, pero duplicándoles el precio.

compraDeluxe :: [Comida] -> Persona -> Persona
compraDeluxe comidas = comprar (superComida comidas)

obtenerSuperComida :: [Comida] -> Comida
obtenerSuperComida = superComida.map (cambiarCosto 200).filter (cuesta 400)

