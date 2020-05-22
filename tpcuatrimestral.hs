import Text.Show.Functions()
import Data.Char
import Data.List (genericLength)

type Desgaste = Float
type Patente = String
type Fecha = (Int, Int, Int)
type Caracteristica = Auto -> Bool
type Tecnico = Auto -> Auto
-- Definiciones base
anio :: Fecha -> Int
anio (_, _, year) = year
 
data Auto = Auto {
 patente :: Patente,
 desgasteLlantas :: [Desgaste],
 rpm :: Int,
 temperaturaAgua :: Int,
 ultimoArreglo :: Fecha
} deriving Show


{-Saber el costo de reparación de un auto
si la patente tiene 7 dígitos, es $ 12.500
si no, si la patente está entre las letras "DJ" y "NB", se aplica el calculoPatental
que es $ 3.000 * la longitud para las patentes que terminen en 4
o $ 20.000 para el resto de las patentes
de lo contrario, se le cobra $ 15000
-}

autoDeTomy :: Auto
autoDeTomy = Auto  "DJV214"  [ 0.5, 0.1, 0, 0.2 ] 1500 90 (10, 2, 2014) 

autoDeAlan :: Auto
autoDeAlan = Auto  "AA7664NB"  [ 0.6, 0.1, 0, 0.2 ] 2700 90 (5, 1, 2020) 

--Punto 1
tamañoPatente :: Auto -> Int
tamañoPatente unAuto= length (patente unAuto)

saberCosto :: Auto -> Int 
saberCosto unAuto 
            | (estaEntreDJyNB unAuto) = calculoPatental unAuto 
            | tamañoPatente unAuto > 7 = 12500
            | otherwise = 15000


empiezaConEaM :: Caracteristica
empiezaConEaM unAuto = elem (primeraLetra unAuto) ['E' .. 'M']

empiezaConN :: Caracteristica
empiezaConN unAuto = elem (take 2 (patente unAuto)) ["NA" , "NB"]

empiezaConD ::  Caracteristica
empiezaConD unAuto = (primeraLetra unAuto) == 'D' && elem (segundaLetra unAuto) [ 'J' .. 'Z'] 

segundaLetra :: Auto -> Char
segundaLetra unAuto = head (tail (patente unAuto))

primeraLetra :: Auto -> Char
primeraLetra unAuto = head (patente unAuto)

estaEntreDJyNB :: Caracteristica
estaEntreDJyNB unAuto=  empiezaConEaM unAuto  || empiezaConN unAuto  || empiezaConD unAuto
            
terminaEn4 :: Caracteristica
terminaEn4 unAuto = last (patente unAuto) == '4'

calculoPatental:: Auto -> Int
calculoPatental unAuto
        | terminaEn4 unAuto = 3000 * (tamañoPatente unAuto)
        | otherwise = 20000

-- Parte 1) Auto peligroso (integrante a)
--Dado un auto, saber si es peligroso. Esta condición se cumple cuando el desgaste de la primera llanta es mayor a 0.5

esAutoPeligroso :: Caracteristica
esAutoPeligroso = (> 0.5).head.desgasteLlantas 


--Parte 2) Necesita revisión (integrante b)
--Dado un auto, saber si necesita revisión. Esta condición se cumple cuando el último arreglo fue realizado en el año 2015 ó antes.

necesitaRevision :: Caracteristica 
necesitaRevision= (>= 2015).anio.ultimoArreglo 

devuelveString :: Auto -> String
devuelveString unAuto 
    |  necesitaRevision unAuto = "No Necesita revision"
    |  otherwise = "Necesita revision"

--Parte 3) 
{-
Necesitamos definir a las siguientes personas que realizan actividades en el taller mecánico:
Alfa: hace que el auto regule a 2.000 vueltas, salvo que esté a menos de 2.000 vueltas, en cuyo caso lo deja como está
Bravo: cambia todas las cubiertas, dejándolas sin desgaste
Charly:  realiza las mismas actividades que Alfa y Bravo
-}


cambiarRevoluciones ::  Int -> Tecnico
cambiarRevoluciones rev unAuto = unAuto {rpm = rev}

cambiarCubiertas :: Tecnico
cambiarCubiertas unAuto = unAuto{desgasteLlantas = [0,0,0,0]}

alfa :: Tecnico
alfa unAuto 
        | rpm unAuto < 2000 = cambiarRevoluciones 2000 unAuto
        | otherwise = unAuto

bravo :: Tecnico
bravo unAuto = cambiarCubiertas unAuto

charly :: Tecnico
charly = alfa.bravo

{-
Parte 2) Integrante b
Necesitamos definir a las siguientes personas que realizan actividades en el taller mecánico
Tango: le gusta decir que hizo muchas cosas pero en realidad no hace ningún arreglo
Zulu: revisa la temperatura del agua, la deja a 90 y hace lo mismo que Lima (ver a continuación)
Lima:  cambia las cubiertas delanteras (las dos primeras), dejándolas sin desgaste. Las posteriores quedan igual
 -}
 
tango:: Tecnico
tango unAuto = unAuto
 
cambiarCubiertasDelanteras :: Tecnico
cambiarCubiertasDelanteras unAuto = unAuto { desgasteLlantas = 0 : 0 : (drop 2 (desgasteLlantas unAuto))}

lima :: Tecnico
lima = cambiarCubiertasDelanteras

zulu :: Tecnico
zulu = lima.cambiarTemperaturaDelAgua 90 

cambiarTemperaturaDelAgua:: Int -> Tecnico
cambiarTemperaturaDelAgua temperatura unAuto = unAuto{temperaturaAgua = temperatura}

{-Punto 4: Ordenamiento TOC de autos

(Común para ambos integrantes) 
Dada una serie de autos, saber si están ordenados en base al siguiente criterio:
los autos ubicados en la posición impar de la lista deben tener una cantidad de desgaste impar
los autos ubicados en la posición par deben tener una cantidad de desgaste par
asumimos que el primer elemento está en la posición 1, el segundo elemento en la posición 2, etc.
La cantidad de desgaste es la sumatoria de desgastes de las cubiertas de los autos multiplicada por 10.
 Ejemplo: 0.2 + 0.5 + 0.6 + 0.1 = 1.4 * 10 = 14. Para determinar si es par o no (y evitar errores de redondeo) 
 es conveniente utilizar la función round.
-}

esImpar :: Int -> Bool
esImpar numero = rem numero 2 /= 0

estanOrdenados :: Auto -> Auto -> Bool
estanOrdenados autoi autop =  esImpar (cantidadDesgaste autoi) && not (esImpar (cantidadDesgaste autop))

cantidadDesgaste :: Auto -> Int
cantidadDesgaste = round.(*10).sum.desgasteLlantas

ordenamiento :: [Auto] -> Bool
ordenamiento ( i : [ ] ) = esImpar (cantidadDesgaste i)
ordenamiento ( i : p : [ ] ) = estanOrdenados i p
ordenamiento ( i : p : autos) = estanOrdenados i p && ordenamiento autos 



{-Punto 5: Orden de reparación
(Común para ambos integrantes) 
Aplicar una orden de reparación, que tiene
una fecha una lista de técnicos
y consiste en que cada uno de los técnicos realice las reparaciones que sabe sobre el auto,
al que además hay que actualizarle la última fecha de reparación.
-}

cambiarFecha :: Fecha -> Tecnico
cambiarFecha unaFecha unAuto = unAuto {ultimoArreglo = unaFecha}

ordenDeReparacion :: Fecha -> [Tecnico] -> Tecnico
ordenDeReparacion unaFecha tecnicos = cambiarFecha unaFecha . (foldl1 (.) tecnicos)
